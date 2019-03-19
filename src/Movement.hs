{-# language TypeFamilies #-}
module Movement where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Class (Graph, Vertex)
import Data.Dependent.Map (DMap)
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity)
import Data.Map (Map)

import qualified Algebra.Graph.Class as Graph
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Graph (scc, topSort)
import qualified Data.Map as Map

import Action
import Pos
import Thing
import ThingType

data Node
  = Node
  { _nodeId :: !ThingType
  , _nodeNewPos :: !(Maybe Pos)
  }
  deriving Show
instance Eq Node where; Node a _ == Node b _ = a == b
instance Ord Node where; compare (Node a _) (Node b _) = compare a b

-- invariant: each Pos can only contain one Thing
buildGraph ::
  (Graph g, Vertex g ~ Node) =>
  (Pos -> Bool) ->
  Map ThingType (Pos, DMap Action Identity) -> g
buildGraph free m = res
  where
    posMap :: Map Pos Node
    (posMap, res) =
      Map.foldrWithKey
        (\k (pos, action) (restPosMap, restGraph) ->
           case moveAction action of
             Nothing ->
               let n = Node k Nothing in
               ( Map.insert pos n restPosMap
               , Graph.overlay (Graph.vertex n) restGraph
               )
             Just actions ->
               let
                 pos' = foldl runMove' pos actions
                 n = Node k $ if free pos' && pos /= pos' then Just pos' else Nothing
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
  AdjacencyMap Node ->
  Map ThingType Pos
runMoves am = foldl' go mempty order
  where
    sccs = Graph.scc am

    -- scc produces an acyclic graph, so topSort always succeeds
    Just order = Graph.topSort sccs

    go :: Map ThingType Pos -> NonEmpty.AdjacencyMap Node -> Map ThingType Pos
    go rest g =
      -- if any vertex cannot be updated to a new position, then that cycle
      -- causes no position changes
      case traverse (\(Node k p) -> (,) k <$> p) (NonEmpty.vertexList1 g) of
        Nothing -> rest
        Just newPositions -> foldr (\(k, v) -> Map.insert k v) rest newPositions
