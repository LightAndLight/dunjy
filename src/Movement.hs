{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
module Movement where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Class (Graph, Vertex)
import Control.Applicative (empty)
import Control.Lens.Fold ((^?))
import Control.Lens.Getter ((^.), uses)
import Control.Lens.Lens (Lens')
import Control.Lens.Setter ((?~), (%=), (.=))
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad (guard)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Set (Set)

import qualified Algebra.Graph.AdjacencyMap as Graph
  (edgeList, vertexSet)
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Graph (scc)
import qualified Algebra.Graph.Class as Graph
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Algebra.Graph.ToGraph as Graph (reachable)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Action
import Pos
import Thing
import ThingType

buildGraph ::
  ( Graph g, Vertex g ~ ThingType
  , HasPos Identity a
  , AsMove b
  ) =>
  Map ThingType a -> -- ^ mobs
  Map ThingType b -> -- ^ mob actions
  g
buildGraph mobs mobActions = res
  where
    posMap :: Map Pos ThingType
    (posMap, res) =
      Map.foldrWithKey
        (\k action (restPosMap, restGraph) ->
           -- firstly, check that the mob doing the action is in our list of mobs
           case Map.lookup k mobs of
             Nothing -> (restPosMap, restGraph)
             Just mob ->
               let pos = mob ^. _pos._Wrapped in
               -- is it doing a move action?
               case action ^? _Move of
                 Nothing ->
                   -- if not, then log its position and insert it as a vertex in the graph
                   --
                   -- if we later find out that something tried to move to this position, then
                   -- the vertex will gain an incoming edge
                   ( Map.insert pos k restPosMap
                   , Graph.overlay (Graph.vertex k) restGraph
                   )
                 Just mv ->
                   let pos' = runMove' pos mv in
                   ( Map.insert pos k restPosMap
                   -- check whether the thing's destination is currently occupied
                   , case Map.lookup pos' posMap of
                       -- if it isn't then it goes in as a vertex
                       Nothing ->
                         Graph.overlay (Graph.vertex k) restGraph
                       -- if it its destination is occupied, then we say that the occupier must
                       -- move before this thing does
                       Just k' ->
                         Graph.overlay (Graph.edge k' k) restGraph
                   ))
        (mempty, Graph.empty)
        mobActions

newtype MovementGraph = MG { unMG :: [[NonEmpty.AdjacencyMap ThingType]] }

class HasMovementGraph s where; _movementGraph :: Lens' s MovementGraph
instance HasMovementGraph MovementGraph where; _movementGraph = id

graphToMG :: AdjacencyMap ThingType -> MovementGraph
graphToMG graph = MG order
  where
    sccs = Graph.scc graph

    roots :: Set (NonEmpty.AdjacencyMap ThingType)
    roots =
      foldr
        (\(_, v) vs -> Set.delete v vs)
        (Graph.vertexSet sccs)
        (Graph.edgeList sccs)

    order :: [[NonEmpty.AdjacencyMap ThingType]]
    order = foldr (\a b -> Graph.reachable a sccs : b) [] roots

makeMovementGraph ::
  (HasPos Identity a, AsMove b) =>
  Map ThingType a -> -- ^ mobs
  Map ThingType b -> -- ^ mob actions
  MovementGraph
makeMovementGraph a b = graphToMG $ buildGraph a b

-- | Move a thing. Report whether it succeeded, along with an updated graph
tryMove ::
  ThingType ->
  MovementGraph ->
  (Bool, MovementGraph)
tryMove _ (MG []) = (False, MG [])
tryMove _ (MG [[]]) = (False, MG [[]])
-- if there's only one root to follow
tryMove n (MG [g:gs])
  -- see if this node is the group of 'things we can move next'
  | NonEmpty.hasVertex n g =
    ( True
    , MG $
      case NonEmpty.removeVertex1 n g of
        -- we successfully moved, and now this group is empty. pop it from the list
        Nothing -> [gs]
        -- successfully moved, but there's more to go
        Just g' -> [g':gs]
    )
  -- it needs to move *now*, but it's either not present or something else needs to move first.
  -- since that thing hasn't moved (it's too slow, or can't move, or won't move), this thing will
  -- not be able to move
  | otherwise =
    ( False
    , didn'tMove n $ MG [g:gs]
    )
tryMove n (MG ([]:gs2:gs)) = tryMove n (MG $ gs2:gs)
tryMove n (MG ((gg:ggs):gs2:gs))
  | NonEmpty.hasVertex n gg =
    ( True
    , MG $
      case NonEmpty.removeVertex1 n gg of
        Nothing -> ggs:gs2:gs
        Just gg' -> (gg':ggs):gs2:gs
    )
  -- if the node isn't in this movement group, we can check the others because
  -- they all stem from roots of the graph
  | otherwise =
    let (res, MG mg') = tryMove n (MG $ gs2:gs) in
    ( res
    , MG $
      -- we should first note that the thing didn't move in this group
      coerce (didn'tMove n $ MG [gg:ggs]) ++
      mg'
    )

-- | report that a certain thing didn't move. this removes it and all its downstream
-- dependencies from the graph
didn'tMove ::
  ThingType ->
  MovementGraph ->
  MovementGraph
didn'tMove _ (MG []) = MG []
didn'tMove n (MG ([]:gs)) = MG . (:) [] . unMG $ didn'tMove n (MG gs)
didn'tMove n (MG ((gg:ggs):gs))
  -- if the node is a member of a (potentially cylic) component, and it didn't move,
  -- then nothing in the cycle can move, and nothing in the chain after it can move
  | NonEmpty.hasVertex n gg = didn'tMove n (MG gs)
  -- if this node isn't part of this group then it might be further down in the chain
  | otherwise =
    MG $
    case unMG $ didn'tMove n (MG $ ggs:gs) of
      [] -> [[gg]]
      ggs':gs' -> (gg:ggs'):gs'

moveThings ::
  ( HasPos Identity a
  , AsMove b
  , Monoid c, UpdatePos c
  , HasMovementGraph s
  , MonadState s m
  ) =>
  Map ThingType a -> -- ^ mobs, to be accessed during the fold
  ThingType -> -- ^ mob id
  b -> -- ^ mob action
  MaybeT m (ThingType, c) -- ^ movement update
moveThings mobs tt action = do
  case action ^? _Move of
    Nothing -> do
      _movementGraph %= didn'tMove tt
      empty
    Just move -> do
      mob <- maybe empty pure $ Map.lookup tt mobs
      let pos' = runMove' (mob ^. _pos._Wrapped) move
      (moved, mg') <- uses _movementGraph (tryMove tt)
      _movementGraph .= mg'
      guard moved
      pure (tt, mempty & _updatePos ?~ pos')
