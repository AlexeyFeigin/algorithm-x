module AlgorithmX where

import Data.IntSet hiding (map, filter, foldl)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List

import Text.Tabular hiding (row)
import Text.Tabular.AsciiArt

type Row = IntSet
type Rows = IntMap Row
type ActiveCols = IntSet
data SparseMatrix = SparseMatrix Rows ActiveCols


(!.) :: Num a => Row -> Key -> a
row !. key
    | member key row = 1
    | otherwise      = 0

toFilledList :: ActiveCols -> Row -> [Int]
toFilledList activeCols row = map (row !.) $ elems activeCols

instance Show SparseMatrix where
    show (SparseMatrix rows activeCols) =
        ('\n' :) $ render show show show $ Table (Group NoLine     $ map Header $ IntMap.keys rows)
                                                 (Group SingleLine $ map Header $ elems activeCols)
                                                 (map (toFilledList activeCols) $ IntMap.elems rows)

printRow :: Row -> IO ()
printRow row = print $ SparseMatrix (IntMap.fromList [(0, row)]) row

oneColumnTable :: String -> [(key, value)] -> Table key String value
oneColumnTable columnHeader ls = Table (Group NoLine $ map (Header . fst) ls)
                                       (Header columnHeader)
                                       (map (\(_, s) -> [s]) ls)

printList :: Show a => String -> [a] -> IO ()
printList title = putStrLn . render show id show . oneColumnTable title . zip [0::Int ..]

printScan :: Show a => [a] -> IO ()
printScan = putStrLn . intercalate "\n\n" . map show


fromSets :: [IntSet] -> SparseMatrix
fromSets sets = SparseMatrix rows activeCols
    where
        rows       = IntMap.fromList $ zip [0..] sets
        activeCols = unions sets


type IsCompleteSolution = Bool

scanAlgoXSimple' :: SparseMatrix
                 -> [Key]
                 -> [([Key], SparseMatrix, IsCompleteSolution)]
scanAlgoXSimple' m@(SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [(solution, m, True)]
    | otherwise =
        (solution, m, False)
            : [ s | (r, row) <- IntMap.toList rows,
                    let m' = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                          (activeCols `difference` row),
                    s <- scanAlgoXSimple' m' (r:solution) ]

algoXSimple' :: SparseMatrix -> [Key] -> [[Key]]
algoXSimple' (SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [solution]
    | otherwise =
        [ s | (r, row) <- IntMap.toList rows,
              let m' = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                    (activeCols `difference` row),
              s <- algoXSimple' m' (r:solution) ]


upToComplete :: [([Key], SparseMatrix, IsCompleteSolution)]
             -> [([Key], SparseMatrix, IsCompleteSolution)]
upToComplete states =
    case break (\(_, _, isComplete) -> isComplete) states of
        (partial, [])           -> partial
        (partial, (complete:_)) -> partial ++ [complete]


(+.) = IntMap.unionWith (+)

algoX m = algoX' m []

algoX' :: SparseMatrix -> [Key] -> [[Key]]
algoX' (SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [solution]
    | otherwise =
        let (c, colSum) = selectedColumn
        in  [ s | colSum > 0,
                  (r, row) <- IntMap.toList rows,
                  member c row,
                  let m' = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                        (activeCols `difference` row),
                  s <- algoX' m' (r:solution) ]
        where
            withOnes row'  = IntMap.fromSet (\_ -> 1) $ row' `intersection` activeCols
            colSums        = IntMap.foldl (\sums row'' -> sums +. withOnes row'')
                                          (IntMap.fromSet (\_ -> 0) activeCols)
                                          rows
            selectedColumn :: (Key, Int)
            selectedColumn = snd . minimum $ map (\(key, cSum) -> (cSum, (key, cSum)))
                                                 (IntMap.toList colSums)

scanAlgoX' :: SparseMatrix -> [Key] -> [([Key], SparseMatrix, IsCompleteSolution)]
scanAlgoX' m@(SparseMatrix rows activeCols) solution
    | size activeCols == 0 = [(solution, m, True)]
    | otherwise =
        let (c, colSum) = selectedColumn
        in  (solution, m, False) :
            [ s | colSum > 0,
                  (r, row) <- IntMap.toList rows,
                  member c row,
                  let m' = SparseMatrix (IntMap.filter (row `disjoint`) rows)
                                        (activeCols `difference` row),
                  s <- scanAlgoX' m' (r:solution) ]
        where
            withOnes row'  = IntMap.fromSet (\_ -> 1) $ row' `intersection` activeCols
            colSums        = IntMap.foldl (\sums row'' -> sums +. withOnes row'')
                                          (IntMap.fromSet (\_ -> 0) activeCols)
                                          rows
            selectedColumn :: (Key, Int)
            selectedColumn = snd . minimum $ map (\(key, cSum) -> (cSum, (key, cSum)))
                                                 (IntMap.toList colSums)
