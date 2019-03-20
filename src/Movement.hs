{-# language BangPatterns #-}
{-# language TypeFamilies #-}
module Movement where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Class (Graph, Vertex)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
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
  , _nodeNewPos :: !(Maybe Pos)
  }
  deriving Show
instance Eq Node where; Node a _ == Node b _ = a == b
instance Ord Node where; compare (Node a _) (Node b _) = compare a b

-- invariant: each Pos can only contain one Thing
buildGraph ::
  (Graph g, Vertex g ~ Node) =>
  (Pos -> Bool) ->
  Map ThingType (Pos, Maybe (NonEmpty Move)) ->
  g
buildGraph free m = res
  where
    posMap :: Map Pos Node
    (posMap, res) =
      Map.foldrWithKey
        (\k (pos, maction) (restPosMap, restGraph) ->
           case maction of
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

    roots :: Set (NonEmpty.AdjacencyMap Node)
    roots =
      foldr
        (\(_, v) vs -> Set.delete v vs)
        (Graph.vertexSet sccs)
        (Graph.edgeList sccs)

    order :: [[NonEmpty.AdjacencyMap Node]]
    order = foldr (\a b -> Graph.reachable a sccs : b) [] roots

    go ::
      Map ThingType Pos ->
      [NonEmpty.AdjacencyMap Node] ->
      Map ThingType Pos
    go !acc [] = acc
    go !acc (root : rest) =
      let
        macc' =
          -- if any vertex cannot be updated to a new position, then that cycle
          -- causes no position changes
          foldr (\(k, v) -> Map.insert k v) acc <$>
          traverse (\(Node k p) -> (,) k <$> p) (NonEmpty.vertexList1 root)
      in
        case macc' of
          Nothing -> acc
          Just acc' -> go acc' rest

moveThings ::
  (Pos -> Bool) ->
  Map ThingType (Pos, Maybe (NonEmpty Move)) ->
  Map ThingType Pos
moveThings free = runMoves . buildGraph free
