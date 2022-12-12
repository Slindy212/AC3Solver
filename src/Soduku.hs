module Soduku where

import AC3Solver
  ( Arc (Arc),
    Cells (..),
    Vals (Vals),
    Var (Var, var),
    applyAC3,
    neighbors,
    v1,
    v2,
    v3,
    v4,
  )
import Control.Monad ()
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe ()
import Data.Set qualified as S
import Test.HUnit
  ( Test (TestCase, TestList),
    assertFailure,
    runTestTT,
    (~:),
    (~?=),
  )
import Test.QuickCheck ()

-- createCellsHelper :: Int -> [Int] -> (Int, Vals)
-- createCellsHelper start [] = (start, Vals [])
-- createCellsHelper start xs = (start, Vals (map Var xs))

--Create a ACSovler Cells Object from an arbitrary list of an unsolved Soduku Board
createCells :: Int -> Vals -> [[Int]] -> Cells
createCells start base board =
  case L.concat board of
    [] -> Cells M.empty
    (x : xs) -> case x of
      0 -> Cells (M.insert start base (cells (createCells (start + 1) base [xs])))
      v -> Cells (M.insert start (Vals [Var v]) (cells (createCells (start + 1) base [xs])))

-- let (start, x') = createCellsHelper start x
--  in Cells (M.fromList [(start, x')])

fullVals :: Vals
fullVals = Vals [v1, v2, v3, v4]

arr1 :: [[Int]]
arr1 =
  [ [2, 0, 4, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 2, 0, 3]
  ]

board2x2 :: Cells
board2x2 =
  Cells
    ( M.fromList
        [ (1, Vals [v2]),
          (2, fullVals),
          (3, Vals [v4]),
          (4, fullVals),
          (5, fullVals),
          (6, fullVals),
          (7, fullVals),
          (8, fullVals),
          (9, fullVals),
          (10, fullVals),
          (11, fullVals),
          (12, fullVals),
          (13, fullVals),
          (14, Vals [v2]),
          (15, fullVals),
          (16, Vals [v3])
        ]
    )

testCreateCells :: Test
testCreateCells =
  "Creating 2 x 2 Cells"
    ~: createCells 1 fullVals arr1 ~?= board2x2

-- >>> runTestTT testCreateCells
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

--Read a txt file representing a board into a completed Cells
createCellsFromTXT :: String -> Cells
createCellsFromTXT = undefined

testCreateCellsFromTXT :: Test
testCreateCellsFromTXT =
  "From Text File"
    ~: createCellsFromTXT "Sodoku" ~?= board2x2

createArcsRow :: Int -> Int -> Int -> [Arc]
createArcsRow cell1 cell2 size | (cell1 == cell2) && (cell2 `mod` size == 0) = []
createArcsRow cell1 cell2 size | cell2 `mod` size == 0 = [Arc (cell1, cell2)]
createArcsRow cell1 cell2 size | cell1 == cell2 = createArcsRow cell1 (cell2 + 1) size
createArcsRow cell1 cell2 size = Arc (cell1, cell2) : createArcsRow cell1 (cell2 + 1) size

-- >>> createArcsRow 16 13 4
-- [Arc {arc = (16,13)},Arc {arc = (16,14)},Arc {arc = (16,15)}]

createArcsCol :: Int -> Int -> Int -> [Arc]
createArcsCol cell1 cell2 size | cell2 > (size * size) = []
createArcsCol cell1 cell2 size | cell2 == (0) = createArcsCol cell1 (cell2 + size) size
createArcsCol cell1 cell2 size | cell1 == cell2 = createArcsCol cell1 (cell2 + size) size
createArcsCol cell1 cell2 size = Arc (cell1, cell2) : createArcsCol cell1 (cell2 + size) size

-- >>> createArcsCol 11 2 9
-- [Arc {arc = (11,2)},Arc {arc = (11,20)},Arc {arc = (11,29)},Arc {arc = (11,38)},Arc {arc = (11,47)},Arc {arc = (11,56)},Arc {arc = (11,65)},Arc {arc = (11,74)}]

getSudokuBox :: Int -> Int -> Int
getSudokuBox j size =
  let i = j - 1
   in let nChunkIndex = i `div` size
       in let row = nChunkIndex `div` (size * size)
           in let col = nChunkIndex `mod` size
               in (col + row * size)

-- >>> getSudokuBox 10 (3)
-- 0

createArcsBox :: Int -> Int -> Int -> Int -> [Arc]
createArcsBox cell1 cell2 box size | cell2 >= (size * size) = []
createArcsBox cell1 cell2 box size | cell1 == ((cell2 `div` size) * (size * size) + (box `div` size) * (size) * (size * size) + (cell2 `mod` size + 1) + ((box `mod` size) * size)) = createArcsBox cell1 (cell2 + 1) box size
createArcsBox cell1 cell2 box size = Arc (cell1, ((cell2 `div` size) * (size * size) + (box `div` size) * (size) * (size * size) + (cell2 `mod` size + 1) + ((box `mod` size) * size))) : createArcsBox cell1 (cell2 + 1) box size

--(cell2 `div` size)*(size*size)
--(box `div` size)*(size)*(size*size)
--(cell2 `mod` size + 1)+((box `mod` size) * size)
-- >>> createArcsBox 2 0 3 2
-- [Arc {arc = (2,11)},Arc {arc = (2,12)},Arc {arc = (2,15)},Arc {arc = (2,16)}]

-- >>> 2 `div` 3
-- 0

createArcsHelper :: Int -> Int -> [Arc]
createArcsHelper cell size | cell > (size * size) * (size * size) = []
createArcsHelper cell size =
  let sizesqr = size * size
   in S.toList (S.fromList ((createArcsRow cell (((cell - 1) `div` sizesqr) * sizesqr + 1) sizesqr) ++ (createArcsCol cell ((cell `mod` sizesqr)) sizesqr) ++ (createArcsBox cell 0 (getSudokuBox cell size) size)))

-- >>> (createArcsHelper 1 2)
-- [Arc {arc = (1,2)},Arc {arc = (1,3)},Arc {arc = (1,4)},Arc {arc = (1,5)},Arc {arc = (1,6)},Arc {arc = (1,9)},Arc {arc = (1,13)}]

createArcsRecur :: Int -> Int -> [Arc]
createArcsRecur start size | start > (size * size * size * size) = []
createArcsRecur start size = createArcsHelper start size ++ createArcsRecur (start + 1) size

--create the Arcs for a Soduku puzzle of board size n^2 by n^2
createArcs :: Int -> [Arc]
createArcs = createArcsRecur 1

-- createArcs = createArcsRecur 1

-- >>> createArcs 2
-- [Arc {arc = (1,2)},Arc {arc = (1,3)},Arc {arc = (1,4)},Arc {arc = (1,5)},Arc {arc = (1,6)},Arc {arc = (1,9)},Arc {arc = (1,13)},Arc {arc = (2,1)},Arc {arc = (2,3)},Arc {arc = (2,4)},Arc {arc = (2,5)},Arc {arc = (2,6)},Arc {arc = (2,10)},Arc {arc = (2,14)},Arc {arc = (3,1)},Arc {arc = (3,2)},Arc {arc = (3,4)},Arc {arc = (3,7)},Arc {arc = (3,8)},Arc {arc = (3,11)},Arc {arc = (3,15)},Arc {arc = (4,1)},Arc {arc = (4,2)},Arc {arc = (4,3)},Arc {arc = (4,7)},Arc {arc = (4,8)},Arc {arc = (4,12)},Arc {arc = (4,16)},Arc {arc = (5,1)},Arc {arc = (5,2)},Arc {arc = (5,6)},Arc {arc = (5,7)},Arc {arc = (5,8)},Arc {arc = (5,9)},Arc {arc = (5,13)},Arc {arc = (6,1)},Arc {arc = (6,2)},Arc {arc = (6,5)},Arc {arc = (6,7)},Arc {arc = (6,8)},Arc {arc = (6,10)},Arc {arc = (6,14)},Arc {arc = (7,3)},Arc {arc = (7,4)},Arc {arc = (7,5)},Arc {arc = (7,6)},Arc {arc = (7,8)},Arc {arc = (7,11)},Arc {arc = (7,15)},Arc {arc = (8,3)},Arc {arc = (8,4)},Arc {arc = (8,5)},Arc {arc = (8,6)},Arc {arc = (8,7)},Arc {arc = (8,12)},Arc {arc = (8,16)},Arc {arc = (9,1)},Arc {arc = (9,5)},Arc {arc = (9,10)},Arc {arc = (9,11)},Arc {arc = (9,12)},Arc {arc = (9,13)},Arc {arc = (9,14)},Arc {arc = (10,2)},Arc {arc = (10,6)},Arc {arc = (10,9)},Arc {arc = (10,11)},Arc {arc = (10,12)},Arc {arc = (10,13)},Arc {arc = (10,14)},Arc {arc = (11,3)},Arc {arc = (11,7)},Arc {arc = (11,9)},Arc {arc = (11,10)},Arc {arc = (11,12)},Arc {arc = (11,15)},Arc {arc = (11,16)},Arc {arc = (12,4)},Arc {arc = (12,8)},Arc {arc = (12,9)},Arc {arc = (12,10)},Arc {arc = (12,11)},Arc {arc = (12,15)},Arc {arc = (12,16)},Arc {arc = (13,1)},Arc {arc = (13,5)},Arc {arc = (13,9)},Arc {arc = (13,10)},Arc {arc = (13,14)},Arc {arc = (13,15)},Arc {arc = (13,16)},Arc {arc = (14,2)},Arc {arc = (14,6)},Arc {arc = (14,9)},Arc {arc = (14,10)},Arc {arc = (14,13)},Arc {arc = (14,15)},Arc {arc = (14,16)},Arc {arc = (15,3)},Arc {arc = (15,7)},Arc {arc = (15,11)},Arc {arc = (15,12)},Arc {arc = (15,13)},Arc {arc = (15,14)},Arc {arc = (15,16)},Arc {arc = (16,4)},Arc {arc = (16,8)},Arc {arc = (16,11)},Arc {arc = (16,12)},Arc {arc = (16,13)},Arc {arc = (16,14)},Arc {arc = (16,15)}]

arcs2x2 :: [Arc]
arcs2x2 =
  S.toList
    ( S.fromList
        ( [ Arc (1, 2),
            Arc (1, 3),
            Arc (1, 4),
            Arc (1, 5),
            Arc (1, 6),
            Arc (1, 9),
            Arc (1, 13),
            Arc (2, 1),
            Arc (2, 3),
            Arc (2, 4),
            Arc (2, 6),
            Arc (2, 10),
            Arc (2, 14),
            Arc (2, 5),
            Arc (3, 1),
            Arc (3, 2),
            Arc (3, 4),
            Arc (3, 7),
            Arc (3, 11),
            Arc (3, 15),
            Arc (3, 8),
            Arc (4, 1),
            Arc (4, 2),
            Arc (4, 3),
            Arc (4, 8),
            Arc (4, 12),
            Arc (4, 16),
            Arc (4, 7),
            Arc (5, 1),
            Arc (5, 2),
            Arc (5, 6),
            Arc (5, 7),
            Arc (5, 8),
            Arc (5, 9),
            Arc (5, 13),
            Arc (6, 1),
            Arc (6, 2),
            Arc (6, 5),
            Arc (6, 7),
            Arc (6, 8),
            Arc (6, 10),
            Arc (6, 14),
            Arc (7, 3),
            Arc (7, 4),
            Arc (7, 5),
            Arc (7, 6),
            Arc (7, 8),
            Arc (7, 11),
            Arc (7, 15),
            Arc (8, 3),
            Arc (8, 4),
            Arc (8, 5),
            Arc (8, 6),
            Arc (8, 7),
            Arc (8, 12),
            Arc (8, 16),
            Arc (9, 10),
            Arc (9, 11),
            Arc (9, 12),
            Arc (9, 13),
            Arc (9, 14),
            Arc (9, 1),
            Arc (9, 5),
            Arc (10, 11),
            Arc (10, 12),
            Arc (10, 14),
            Arc (10, 13),
            Arc (10, 2),
            Arc (10, 6),
            Arc (10, 9),
            Arc (11, 12),
            Arc (11, 15),
            Arc (11, 16),
            Arc (11, 3),
            Arc (11, 7),
            Arc (11, 9),
            Arc (11, 10),
            Arc (12, 4),
            Arc (12, 8),
            Arc (12, 9),
            Arc (12, 10),
            Arc (12, 11),
            Arc (12, 16),
            Arc (12, 15),
            Arc (13, 14),
            Arc (13, 15),
            Arc (13, 16),
            Arc (13, 1),
            Arc (13, 5),
            Arc (13, 9),
            Arc (13, 10),
            Arc (14, 2),
            Arc (14, 6),
            Arc (14, 10),
            Arc (14, 9),
            Arc (14, 13),
            Arc (14, 15),
            Arc (14, 16),
            Arc (15, 3),
            Arc (15, 7),
            Arc (15, 11),
            Arc (15, 13),
            Arc (15, 14),
            Arc (15, 12),
            Arc (15, 16),
            Arc (16, 4),
            Arc (16, 8),
            Arc (16, 12),
            Arc (16, 13),
            Arc (16, 14),
            Arc (16, 15),
            Arc (16, 11)
          ]
        )
    )

testCreateArcs :: Test
testCreateArcs =
  "Create 2x2 Arcs"
    ~: (createArcs 2) ~?= (arcs2x2)

-- >>> runTestTT testCreateArcs
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

solvedArr1 :: [[Int]]
solvedArr1 =
  [ [2, 3, 4, 1],
    [1, 4, 3, 2],
    [3, 1, 2, 4],
    [4, 2, 1, 3]
  ]

emptyArr1 :: [[Int]]
emptyArr1 =
  [ [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0]
  ]

boardToPuzzleHelper :: Vals -> Maybe Int
boardToPuzzleHelper (Vals x) | length x > 1 = Nothing
boardToPuzzleHelper (Vals x) | length x == 1 = Just (var (head x))
boardToPuzzleHelper _ = Nothing

boardToPuzzle :: Cells -> Maybe [Int]
boardToPuzzle (Cells c) | null (M.toList c) = Just []
boardToPuzzle (Cells c) =
  let list = M.toList c
   in let (i, v) = head list
       in case boardToPuzzleHelper v of
            Nothing -> Nothing
            Just v' -> case boardToPuzzle (Cells (M.fromList (tail list))) of
              Nothing -> Nothing
              Just vs' -> Just (v' : vs')

chunk :: Int -> Maybe [a] -> [[a]]
chunk _ (Just []) = []
chunk _ Nothing = []
chunk i (Just xs) | length xs < i = [xs]
chunk i (Just xs) = take i xs : chunk i (Just (reverse (take (length xs - i) (reverse xs))))

-- >>> chunk 4 (boardToPuzzle (createCells 1 fullVals solvedArr1))
-- [[2,3,4,1],[1,4,3,2],[3,1,2,4],[4,2,1,3]]

test :: [[Int]]
test =
  [ [2, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0]
  ]

test1 :: Vals
test1 = Vals [v2]

test2 :: Vals
test2 = Vals [v1, v2, v3, v4]

c1 :: Cells
c1 = createCells 1 fullVals arr1

sol :: Cells
sol = applyAC3 c1 arcs2x2

n :: [Arc]
n = neighbors 13 arcs2x2

-- >>> removeInconsistentValue test2 test1
-- (Vals {vals = [Var {var = 1},Var {var = 3},Var {var = 4}]},True)

-- >>> boardToPuzzle sol
-- Just [2,3,4,1,1,4,3,2,3,1,2,4,4,2,1,3]

solveSodukuPuzzle :: [[Int]] -> Maybe [[Int]]
solveSodukuPuzzle is =
  let board = createCells 1 fullVals is
   in let sol = applyAC3 board arcs2x2
       in let puzzle = boardToPuzzle sol
           in case puzzle of
                Nothing -> Nothing
                Just x -> Just (chunk 4 (Just x))

-- >>> solveSodukuPuzzle arr1
-- Just [[2,3,4,1],[1,4,3,2],[3,1,2,4],[4,2,1,3]]

solveSodukuPuzzleSTR :: String -> Maybe [[Int]]
solveSodukuPuzzleSTR = undefined

testSolver :: Test
testSolver =
  "Test Solvers"
    ~: [ solveSodukuPuzzle arr1 ~?= Just solvedArr1,
         solveSodukuPuzzleSTR "Sodoku" ~?= Just solvedArr1,
         solveSodukuPuzzle emptyArr1 ~?= Nothing
       ]
