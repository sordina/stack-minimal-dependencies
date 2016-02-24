{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import Data.Tuple
import Language.Dot.Parser
import Language.Dot.Syntax
import qualified Data.Set as S

main :: IO ()
main = getContents >>= print . fmap go . parseDot "-"

data Node = Node deriving (Eq, Ord, Show)
type Edge = (Node,Node)

go :: Graph -> S.Set Node
go ns = target_nodes (getNodes ns) (getEdges ns)

getNodes :: Graph -> [Node]
getNodes = undefined

getEdges :: Graph -> [Edge]
getEdges = undefined

target_nodes :: [Node] -> [Edge] -> S.Set Node
target_nodes nodes edges = fixedpoint (rewrite (reverseMap edges)) (S.fromList nodes)

rewrite :: (Node -> Maybe Node) -> S.Set Node -> S.Set Node
rewrite f s = undefined

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
