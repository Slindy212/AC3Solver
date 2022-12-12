module AC3Solver where

import Control.Monad ()
import Data.Map qualified as M
import Data.Maybe ()
import Test.HUnit
  ( Test (TestCase, TestList),
    assertFailure,
    runTestTT,
    (~:),
    (~?=),
  )
import Test.QuickCheck ()

-- A List of all possible variable values for a cell
newtype Vals = Vals {vals :: [Var]} deriving (Eq, Ord, Show)

--A list of Vals representing all possible solutions each cell
newtype Cells = Cells {cells :: M.Map Int Vals} deriving (Eq, Ord, Show)

--Tuples which represent two locations that cannot be equal
newtype Arc = Arc {arc :: (Int, Int)} deriving (Eq, Ord, Show)

--For this case, variables are just integers
newtype Var = Var {var :: Int} deriving (Eq, Ord, Show)

v1 :: Var
v1 = Var 1

v2 :: Var
v2 = Var 2

v3 :: Var
v3 = Var 3

v4 :: Var
v4 = Var 4

v5 :: Var
v5 = Var 5

vl1 :: Vals
vl1 = Vals [v1, v2, v3]

vl2 :: Vals
vl2 = Vals [v2, v3]

vl3 :: Vals
vl3 = Vals [v3]

b1 :: Cells
b1 = Cells (M.fromList [(1, vl1), (2, vl2)])

a1 :: Arc
a1 = Arc (1, 2)

--Return an arc with Var removed from Cells
removeValue :: Vals -> Var -> Vals
removeValue vs v = Vals (filter (/= v) (vals vs))

testRV :: Test
testRV =
  "removeValue"
    ~: [ removeValue vl1 v1 ~?= Vals [v2, v3],
         removeValue vl2 v1 ~?= vl2
       ]

-- >>> runTestTT testRV
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

--Return first list of vals wwith inconsistent values compared to second vals removed
removeInconsistentValue :: Vals -> Vals -> (Vals, Bool)
removeInconsistentValue vs1 vs2 | length (vals vs2) > 1 = (vs1, False)
removeInconsistentValue (Vals (v : vs1)) (Vals vs2) =
  let (v', b) = removeInconsistentValue (Vals vs1) (Vals vs2)
   in if v `elem` vs2 then (v', True) else (Vals (v : vals v'), b)
removeInconsistentValue (Vals []) _ = (Vals [], False)

testRIV :: Test
testRIV =
  "removeInconsistentValue"
    ~: [ removeInconsistentValue vl1 vl3 ~?= (Vals [v1, v2], True),
         removeInconsistentValue vl2 vl3 ~?= (Vals [v2], True),
         removeInconsistentValue vl1 vl2 ~?= (Vals [v1, v2, v3], False),
         removeInconsistentValue (Vals []) vl2 ~?= (Vals [], False)
       ]

-- >>> runTestTT testRIV
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

b1_solved :: Cells
b1_solved = Cells (M.fromList [(1, Vals [v1]), (2, Vals [v3])])

isOne :: Vals -> Bool
isOne (Vals []) = False
isOne (Vals v) = length v == 1

--Checks to see if there is only one remaining value for each cell
solved :: Cells -> Bool
solved (Cells c) =
  case M.toList c of
    [] -> True
    (i, v) : cs -> isOne v && solved (Cells (M.fromList cs))

testSolved :: Test
testSolved =
  "solved"
    ~: [ solved b1 ~?= False,
         solved b1_solved ~?= True
       ]

-- >>> runTestTT testSolved
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

--Input a cell, and returns a list of all cells that have an arc with that cell
neighbors :: Int -> [Arc] -> [Arc]
neighbors i (Arc (i1, i2) : as)
  | i1 == i =
    Arc (i1, i2) : neighbors i as
  | i2 == i =
    Arc (i1, i2) : neighbors i as
  | otherwise =
    neighbors i as
neighbors i [] = []

testNeighbors :: Test
testNeighbors =
  "solved"
    ~: neighbors 1 [a1] ~?= [Arc (1, 2)]

-- >>> runTestTT testNeighbors
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

--Go through each cell tuple in the list of arcs
--Remove the inconsistent values between the two cells
--If a value was removed re-check the inconsistent values of all neighbors to the adjusted cell
--Return a new board with all inconsitent values removed
applyAC3sub :: Cells -> [Arc] -> [Arc] -> Cells
applyAC3sub (Cells c) orgarcs [] = Cells c
applyAC3sub (Cells c) orgarcs ((Arc (a1, a2)) : as) =
  let v1 = M.lookup a1 c
   in let v2 = M.lookup a2 c
       in case (v1, v2) of
            (Nothing, _) -> applyAC3sub (Cells c) orgarcs as
            (_, Nothing) -> applyAC3sub (Cells c) orgarcs as
            (Just vals1, Just vals2) ->
              case removeInconsistentValue vals1 vals2 of
                (_, False) -> applyAC3sub (Cells c) orgarcs as
                (vals1', True) -> applyAC3sub (Cells (M.insert a1 vals1' c)) orgarcs (as ++ neighbors a1 orgarcs)

applyAC3 :: Cells -> [Arc] -> Cells
applyAC3 (Cells c) orgarcs = applyAC3sub (Cells c) orgarcs orgarcs

val1 :: Vals
val1 = Vals [v1, v2]

val2 :: Vals
val2 = Vals [v2, v3]

val3 :: Vals
val3 = Vals [v3, v4]

val4 :: Vals
val4 = Vals [v4, v5]

-- val3' :: Vals
-- (val3', _) = (removeInconsistentValue val3 val4)

-- >>> removeInconsistentValue val3' val4
-- (Vals {vals = [Var 3]},False)

board1 :: Cells
board1 = Cells (M.fromList [(1, val1), (2, val2), (3, val3), (4, val4)])

arc1 :: Arc
arc1 = Arc (1, 2)

arc2 :: Arc
arc2 = Arc (2, 3)

arc3 :: Arc
arc3 = Arc (3, 4)

testApplyAC3 :: Test
testApplyAC3 =
  "AC3"
    ~: applyAC3 board1 [arc1, arc2, arc3] ~?= Cells (M.fromList [(1, Vals [v1]), (2, Vals [v2]), (3, Vals [v3]), (4, Vals [v4])])

-- >>> runTestTT testApplyAC3
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

-- >>> applyAC3 board1 [arc1, arc2, arc3]
-- Cells {cells = fromList [(1,Vals {vals = [Var 1,Var 2]}),(2,Vals {vals = [Var 2,Var 3]}),(3,Vals {vals = [Var 3]}),(4,Vals {vals = [Var 4]})]}

--Use the AC3 algorithm
--If the board is not solved, choose a cell with unsolved values, and guess one of the remaining options
--Check to see if the board is solved
--Return the solved Cells

instantiate :: Vals -> [Vals]
instantiate (Vals []) = []
instantiate (Vals (x : xs)) = Vals [x] : instantiate (Vals xs)

instantiate2Helper :: [Vals] -> Cells -> Int -> [Cells]
instantiate2Helper [] (Cells c) i = []
instantiate2Helper (v : vs) (Cells c) i = Cells (M.insert i v c) : instantiate2Helper vs (Cells c) i

instantiate2 :: Cells -> Int -> [Cells]
instantiate2 (Cells c) i =
  case M.lookup i c of
    Nothing -> []
    Just v ->
      let vals = instantiate v
       in instantiate2Helper vals (Cells c) i

instantiate3 :: Cells -> [Int] -> [Cells]
instantiate3 (Cells c) [] = []
instantiate3 (Cells c) (i : is) =
  instantiate2 (Cells c) i ++ instantiate3 (Cells c) is

-- applyAC3All :: [Cells] -> [Arc] -> Maybe Cells

-- applyAC3All [] arcs = Nothing
-- applyAC3All (c : cs) arcs =
-- let sol = applyAC3Guess c arcs
--  in case sol of
--       Nothing -> applyAC3All cs arcs
--       Just s -> Just s

-- applyAC3Guess :: Cells -> [Arc] -> Maybe Cells
-- applyAC3Guess (Cells cells) arcs =
--   let sol = applyAC3 (Cells cells) arcs
--    in if solved sol then Just (Cells cells)
--         else
--           ( let ks = M.keys cells
--              in let cs = instantiate3 (Cells cells) ks
--                  in applyAC3All cs arcs
--           )

val4_changed :: Vals
val4_changed = Vals [v1, v2, v3, v4]

board2 :: Cells
board2 = Cells (M.fromList [(1, val1), (2, val2), (3, val3), (4, val4_changed)])

-- testApplyAC3Guess :: Test
-- testApplyAC3Guess =
--   "AC3Guess"
--     ~: applyAC3Guess board1 [arc1, arc2, arc3] ~?= Just (Cells (M.fromList [(1, Vals [v1]), (2, Vals [v2]), (3, Vals [v3]), (4, Vals [v4])]))
