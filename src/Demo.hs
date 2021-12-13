module Demo where

import AlgorithmX

import Data.IntSet hiding (map, filter, foldl)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List

set0 :: IntSet
set0 = fromList [1, 4, 7]

sets1 :: [IntSet]
sets1 = map fromList [ [1, 4, 7]
                     , [1, 4]
                     , [4, 5, 7]
                     , [3, 5, 6]
                     , [2, 3, 6, 7]
                     , [2, 7]
                     ]

rows1 = IntMap.fromList $ zip [0..] sets1

m1 = SparseMatrix rows1 (unions rows1)
