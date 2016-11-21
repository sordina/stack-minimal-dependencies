{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Main where

-- import Lib
import Data.Tuple
import Data.Maybe
import Language.Dot.Parser
import Language.Dot.Syntax
import qualified Data.Set as S

main :: IO ()
main = getContents >>= either print (mapM_ (putStrLn . unNode)) . fmap S.toList . fmap go . parseDot "-"

test, test2, test3, test4 :: IO ()
test     = print $ parseDot "test" testdata
test2    = print $ fmap go $ parseDot "test2" testdata
test3    = print $ fmap getGraphNodes $ parseDot "test2" testdata
test4    = print $ fmap getGraphEdges $ parseDot "test2" testdata

testdata :: [Char]
testdata = "digraph { a -> b; a -> c; d -> e; }"

data Node = Node String deriving (Eq, Ord, Show)
type Edge = (Node,Node)

unNode :: Node -> String
unNode (Node n) = takeWhile (/= '"') $ tail $ dropWhile (/= '"') n

go :: Graph -> S.Set Node
go ns = target_nodes (plainNodes ++ extraNodes) (getGraphEdges ns)
  where
    plainEdges = getGraphEdges ns
    plainNodes = getGraphNodes ns
    extraNodes = plainEdges >>= edgeToNodes

edgeToNodes :: forall t. (t, t) -> [t]
edgeToNodes (a,b) = [a,b]

getGraphNodes :: Graph -> [Node]
getGraphNodes (Graph _ _ _ ss) = ss >>= getStatementNodes

getStatementNodes :: Statement -> [Node]
getStatementNodes ( SubgraphStatement subgraph ) = getSubgraphStatements subgraph >>= getStatementNodes
getStatementNodes ( NodeStatement     nid _    ) = [getNodeInfo nid]
getStatementNodes ( EdgeStatement _ _          ) = []
getStatementNodes ( AttributeStatement _ _     ) = []
getStatementNodes ( AssignmentStatement _ _    ) = []

getSubgraphStatements :: Subgraph -> [Statement]
getSubgraphStatements ( NewSubgraph _ ss ) = ss
getSubgraphStatements ( SubgraphRef _      ) = []

getNodeInfo :: NodeId -> Node
getNodeInfo (NodeId i _) = Node (show i)

getGraphEdges :: Graph -> [Edge]
getGraphEdges (Graph _ _ _ ss) = ss >>= getStatementEdges

getStatementEdges :: Statement -> [Edge]
getStatementEdges ( SubgraphStatement subgraph ) = getSubgraphStatements subgraph >>= getStatementEdges
getStatementEdges ( NodeStatement _ _          ) = []
getStatementEdges ( EdgeStatement es _         ) = getEdges es
getStatementEdges ( AttributeStatement _ _     ) = []
getStatementEdges ( AssignmentStatement _ _    ) = []

getEdges :: [Entity] -> [(Node, Node)]
getEdges ( [ ENodeId NoEdge       (NodeId f _)
           , ENodeId DirectedEdge (NodeId t _)
           ] ) = [(Node (show f), Node (show t))]
getEdges _     = []

target_nodes :: [Node] -> [Edge] -> S.Set Node
target_nodes nodes edges = fixedpoint (rewrite (reverseMap edges)) (S.fromList nodes)

rewrite :: (Node -> Maybe Node) -> S.Set Node -> S.Set Node
rewrite f = S.map g
  where
  g n = fromMaybe n (f n)

reverseMap :: (Eq b, Ord b) => [(a,b)] -> b -> Maybe a
reverseMap e = flip lookup (map swap e)

-- Thanks SO: http://stackoverflow.com/questions/6300216/transforming-a-function-that-computes-a-fixed-point
--
fixedpoint :: forall a. Eq a => (a -> a) -> a -> a
fixedpoint = findFixedPoint

findFixedPoint :: forall a. Eq a => (a -> a) -> a -> a
findFixedPoint f = fst . head
                 . dropWhile (uncurry (/=))
                 . pairwise (,)
                 . iterate f

pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise _ []           = []
pairwise _ (_:[])       = []
pairwise f (x:(xs:xss)) = f x xs:pairwise f xss
