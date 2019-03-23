{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
module Movement where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Class (Graph, Vertex)
import Control.Lens.Fold ((^?))
import Control.Lens.Setter ((?~))
import Control.Monad.State (execState, gets, modify)
import Data.Foldable (foldl')
import Data.Function ((&))
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
  , AsMove a
  ) =>
  (Pos -> Bool) ->
  Map ThingType Pos ->
  Map ThingType a ->
  g
buildGraph free locations m = res
  where
    posMap :: Map Pos Node
    (posMap, res) =
      Map.foldrWithKey
        (\k action (restPosMap, restGraph) ->
           case Map.lookup k locations of
             Nothing -> (restPosMap, restGraph)
             Just pos ->
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
                    then (Set.insert p s, Map.insert (_nodeId node) (mempty & updatePos_ ?~ p) m)
                    else (Set.insert (_nodeCurrentPos node) s, m)
                  Nothing -> (Set.insert (_nodeCurrentPos node) s, m))
            (NonEmpty.vertexList1 root)

moveThings ::
  ( Monoid b, UpdatePos b
  , AsMove a
  ) =>
  (Pos -> Bool) ->
  Map ThingType Pos ->
  Map ThingType a ->
  Map ThingType b
moveThings free locs = runMoves . buildGraph free locs
