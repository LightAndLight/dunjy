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
import Control.Monad.State (MonadState, execState, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Coerce (coerce)
import Data.Foldable (foldl')
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

data Node
  = Node
  { _nodeId :: !ThingType
  , _nodeCurrentPos :: !Pos
  , _nodeNewPos :: !(Maybe Pos)
  }
  deriving Show
instance Eq Node where; Node a _ _ == Node b _ _ = a == b
instance Ord Node where; compare (Node a _ _) (Node b _ _) = compare a b

-- invariant: each Pos can only contain one Thing
buildGraph ::
  ( Graph g, Vertex g ~ Node
  , HasPos Identity a
  , AsMove b
  ) =>
  (Pos -> Bool) -> -- ^ which tiles are free
  Map ThingType a -> -- ^ mobs
  Map ThingType b -> -- ^ mob actions
  g
buildGraph free mobs m = res
  where
    posMap :: Map Pos Node
    (posMap, res) =
      Map.foldrWithKey
        (\k action (restPosMap, restGraph) ->
           case Map.lookup k mobs of
             Nothing -> (restPosMap, restGraph)
             Just mob ->
               let pos = mob ^. _pos._Wrapped in
               case action ^? _Move of
                 Nothing ->
                   let
                     n = Node { _nodeId = k, _nodeCurrentPos = pos, _nodeNewPos = Nothing }
                   in
                   ( Map.insert pos n restPosMap
                   , Graph.overlay (Graph.vertex n) restGraph
                   )
                 Just mv ->
                   let
                     pos' = runMove' pos mv
                     n =
                       Node
                       { _nodeId = k
                       , _nodeCurrentPos = pos
                       , _nodeNewPos = if free pos' && pos /= pos' then Just pos' else Nothing
                       }
                   in
                     ( Map.insert pos n restPosMap
                     , case Map.lookup pos' posMap of
                         Nothing ->
                           Graph.overlay (Graph.vertex n) restGraph
                         Just n' ->
                           Graph.overlay (Graph.edge n' n) restGraph
                     ))
        (mempty, Graph.empty)
        m

runMoves ::
  forall a.
  (Monoid a, UpdatePos a) =>
  AdjacencyMap Node -> -- ^ movement dependency graph
  Map ThingType a -- ^ position updates
runMoves am = snd $ foldl' go (mempty, mempty) order
  where
    sccs = Graph.scc am

    roots :: Set (NonEmpty.AdjacencyMap Node)
    roots =
      foldr
        (\(_, v) vs -> Set.delete v vs)
        (Graph.vertexSet sccs)
        (Graph.edgeList sccs)

    order :: [[NonEmpty.AdjacencyMap Node]]
    order = foldr (\a b -> Graph.reachable a sccs : b) [] roots

    go ::
      (Set Pos, Map ThingType a) ->
      [NonEmpty.AdjacencyMap Node] ->
      (Set Pos, Map ThingType a)
    go = foldl' go'
      where
        go' ::
          (Set Pos, Map ThingType a) ->
          NonEmpty.AdjacencyMap Node ->
          (Set Pos, Map ThingType a)
        go' (!occupied, !acc) root =
          flip execState (occupied, acc) $
          traverse
            (\node -> do
              seen <- gets fst
              modify $ \(s, m) ->
                case _nodeNewPos node of
                  Just p ->
                    if p `Set.notMember` seen
                    then (Set.insert p s, Map.insert (_nodeId node) (mempty & _updatePos ?~ p) m)
                    else (Set.insert (_nodeCurrentPos node) s, m)
                  Nothing -> (Set.insert (_nodeCurrentPos node) s, m))
            (NonEmpty.vertexList1 root)

moveThings ::
  ( HasPos Identity a
  , AsMove b
  , Monoid c, UpdatePos c
  ) =>
  (Pos -> Bool) ->
  Map ThingType a -> -- ^ mobs
  Map ThingType b -> -- ^ mob actions
  Map ThingType c -- ^ movement updates
moveThings free locs = runMoves . buildGraph free locs

buildGraph' ::
  ( Graph g, Vertex g ~ ThingType
  , HasPos Identity a
  , AsMove b
  ) =>
  Map ThingType a -> -- ^ mobs
  Map ThingType b -> -- ^ mob actions
  g
buildGraph' mobs mobActions = res
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

makeMovementGraph :: AdjacencyMap ThingType -> MovementGraph
makeMovementGraph graph = MG order
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

moveThings' ::
  ( HasPos Identity a
  , AsMove b
  , Monoid c, UpdatePos c
  , HasMovementGraph s
  , MonadState s m
  ) =>
  (Pos -> Bool) -> -- ^ which positions are unoccupied by scenery
  Map ThingType a -> -- ^ mobs, to be accessed during the fold
  ThingType -> -- ^ mob id
  b -> -- ^ mob action
  MaybeT m c -- ^ movement update
moveThings' frees mobs tt action = do
  case action ^? _Move of
    Nothing -> do
      _movementGraph %= didn'tMove tt
      empty
    Just move -> do
      mob <- maybe empty pure $ Map.lookup tt mobs
      let pos' = runMove' (mob ^. _pos._Wrapped) move
      guard (frees pos')
      (moved, mg') <- uses _movementGraph (tryMove tt)
      _movementGraph .= mg'
      guard moved
      pure $ mempty & _updatePos ?~ pos'
