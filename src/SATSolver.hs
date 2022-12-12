module SATSolver where

import Control.Monad
import Data.Bits (Bits (xor))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import GHC.Float (fromRat'')
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck
  ( Arbitrary (..),
    Args (maxSuccess),
    Gen,
    Property,
    Testable (property),
    choose,
    collect,
    elements,
    frequency,
    quickCheckWith,
    sample',
    sized,
    stdArgs,
    (.&&.),
    (===),
  )

import AC3Solver qualified as AC3
import Soduku qualified

---------------------------------------------------------------------------
-- Basic types

-- | An expression in CNF (conjunctive normal form) is a conjunction
-- of clauses. We store these clauses in the conjunction in a list.
data CNF = Conj {clauses :: [Clause], constraints :: [Bool]} deriving (Eq, Ord, Show)

-- | A clause is a disjunction of a number of literals, again storing
-- each literal in a list.
newtype Clause = Disj {lits :: [Lit]} deriving (Eq, Ord, Show)

-- | A literal is either a positive or a negative variable
data Lit = Lit {polarity :: Bool, var :: Var} deriving (Eq, Ord, Show)

-- | A variable is just a character
newtype Var = Var Char
  deriving (Eq, Ord, Show)

-- A few variables for test cases
vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

exampleFormula :: CNF
exampleFormula =
  Conj
    [ Disj [Lit True vA, Lit True vB, Lit True vC],
      Disj [Lit False vA],
      Disj [Lit False vB, Lit True vC]
    ]

-------------------------------------------------------------------------

-- | Is the literal positive?
isPos :: Lit -> Bool
isPos = polarity

-- | Negate a literal
neg :: Lit -> Lit
neg (Lit b x) = Lit (not b) x

instance Enum Var where
  toEnum i = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

-- | A long list of variables
allVars :: [Var]
allVars =
  [ Var 'A',
    Var 'B',
    Var 'C',
    Var 'D',
    Var 'E',
    Var 'F',
    Var 'G',
    Var 'H',
    Var 'I',
    Var 'J',
    Var 'K',
    Var 'L',
    Var 'M',
    Var 'N',
    Var 'O',
    Var 'P',
    Var 'Q',
    Var 'R',
    Var 'S',
    Var 'T',
    Var 'U',
    Var 'V',
    Var 'W',
    Var 'X',
    Var 'Y',
    Var 'Z'
  ]

-- >>>  countVars exampleFormula
-- fromList [(Var 'A',2),(Var 'B',2),(Var 'C',2)]
countVars :: CNF -> Map Var Int
-- countVars cnf = Map.fromListWith (+) (\x -> (x,1)) (map var (concat (map lits (clauses cnf))))
countVars cnf = Map.fromListWith (+) (map ((\x -> (x, 1)) . var) (concatMap lits (clauses cnf)))

-- | All of the variables that appear anywhere in the formula, in sorted order
-- >>> vars exampleFormula
-- WAS [Var 'A',Var 'B',Var 'C']
-- NOW [Var 'A',Var 'B',Var 'C']
vars :: CNF -> [Var]
vars cnf = List.nub (map var (concatMap lits (clauses cnf)))

testCountVars :: Test
testCountVars =
  "countVars"
    ~: countVars exampleFormula ~?= Map.fromList [(vA, 2), (vB, 2), (vC, 2)]

testVars :: Test
testVars =
  "vars"
    ~: vars exampleFormula ~?= [vA, vB, vC]

-------------------------------------------------------------------------

-- >>> sample' (genCNF 3)
-- [Conj {clauses = []},Conj {clauses = []},Conj {clauses = [Disj {lits = []},Disj {lits = [Lit {polarity = True, var = Var 'B'}]}]},Conj {clauses = []},Conj {clauses = [Disj {lits = [Lit {polarity = False, var = Var 'B'}]}]},Conj {clauses = [Disj {lits = [Lit {polarity = False, var = Var 'A'},Lit {polarity = True, var = Var 'B'}]},Disj {lits = [Lit {polarity = False, var = Var 'B'}]}]},Conj {clauses = []},Conj {clauses = [Disj {lits = [Lit {polarity = True, var = Var 'A'}]},Disj {lits = [Lit {polarity = False, var = Var 'A'},Lit {polarity = False, var = Var 'C'}]},Disj {lits = [Lit {polarity = False, var = Var 'C'}]}]},Conj {clauses = [Disj {lits = [Lit {polarity = False, var = Var 'B'},Lit {polarity = True, var = Var 'B'}]},Disj {lits = [Lit {polarity = True, var = Var 'C'}]}]},Conj {clauses = [Disj {lits = [Lit {polarity = True, var = Var 'C'},Lit {polarity = True, var = Var 'A'}]},Disj {lits = [Lit {polarity = False, var = Var 'A'}]}]},Conj {clauses = [Disj {lits = [Lit {polarity = True, var = Var 'A'}]},Disj {lits = [Lit {polarity = False, var = Var 'B'}]},Disj {lits = [Lit {polarity = False, var = Var 'A'},Lit {polarity = True, var = Var 'A'},Lit {polarity = False, var = Var 'C'},Lit {polarity = True, var = Var 'B'}]},Disj {lits = [Lit {polarity = False, var = Var 'A'},Lit {polarity = True, var = Var 'B'},Lit {polarity = True, var = Var 'B'}]}]}]

-- | Generate a random variable (limited to the first `n` variables).
genVar :: Int -> Gen Var
genVar n | n < 1 = error "Must supply a positive number to genVar"
genVar n =
  elements
    ( take
        n
        allVars
    )

-- | Generate a random literal with `n` distinct variables.
genLit :: Int -> Gen Lit
genLit n = do
  t <- choose (True, False)
  ts <- genVar n
  return (Lit t ts)

-- | Generate a random Clause with `n` distinct variables.
genClause :: Int -> Gen Clause
genClause n = do
  t <- genListLits n
  return (Disj t)

genListLits :: Int -> Gen [Lit]
genListLits n = sized gen
  where
    gen a =
      frequency
        [ (1, return []),
          (a, liftM2 (:) (genLit n) (gen (n `div` 2)))
        ]

-- | Generate a random CNF with `n` distinct variables.
genCNF :: Int -> Gen CNF
genCNF n = do
  t <- genListClauses n
  return (Conj t)

genListClauses :: Int -> Gen [Clause]
genListClauses n = sized gen
  where
    gen a =
      frequency
        [ (1, return []),
          (a, liftM2 (:) (genClause n) (gen (n `div` 2)))
        ]

-- make sure that genVars produces the right number of variables.
testGenVars :: Test
testGenVars =
  "genVars" ~: do
    xs <- sample' (genVar 3)
    return $ length (List.nub xs) == 3

-- make sure that arbitrary formulae don't contain too many variables.
testGenCNF :: Test
testGenCNF =
  "genCNF num vars" ~: do
    xs <- sample' (genCNF defaultNumVariables)
    return $ all (\c -> length (countVars c) <= defaultNumVariables) xs

-- make sure that we generate different formulae.
testGenCNFdiff :: Test
testGenCNFdiff =
  "genCNF diff" ~: do
    xs <- sample' (genCNF defaultNumVariables)
    return (length (List.nub xs) >= 8)

defaultNumVariables :: Int
defaultNumVariables = 7

instance Arbitrary Var where
  arbitrary = genVar defaultNumVariables
  shrink v
    | v == vA = []
    | otherwise = [vA .. pred v]

instance Arbitrary Lit where
  arbitrary = genLit defaultNumVariables
  shrink (Lit b v) =
    map (`Lit` v) (shrink b)
      ++ map (Lit b) (shrink v)

instance Arbitrary Clause where
  arbitrary = genClause defaultNumVariables
  shrink (Disj l) = map Disj (shrink l)

instance Arbitrary CNF where
  arbitrary = genCNF defaultNumVariables
  shrink (Conj x) = map Conj (shrink x)

---------------------------------------------------------------------
-- Satisfiable and unsatisfiable formulae

--  A /\ (not A)
unSatFormula :: CNF
unSatFormula = Conj [Disj [Lit True vA], Disj [Lit False vA]]

-- | Assignments of values to (some) variables
type Valuation = Map Var Bool

-- | No bindings
emptyValuation :: Valuation
emptyValuation = Map.empty

-- | Add a new binding
extend :: Var -> Bool -> Valuation -> Valuation
extend = Map.insert

-- | Check the value of a variable
value :: Var -> Valuation -> Maybe Bool
value = Map.lookup

-- | Create a valuation from a given list of bindings
fromList :: [(Var, Bool)] -> Valuation
fromList = Map.fromList

exampleValuation :: Valuation
exampleValuation = Map.fromList [(vA, False), (vB, True), (vC, True)]

satisfiesLit :: Valuation -> Lit -> Bool
satisfiesLit a lit = a Map.!? var lit == Just (polarity lit)


--Satisfies now must also satisify all bools
satisfies :: Valuation -> CNF -> Bool
satisfies a cnf = all (any (satisfiesLit a) . lits) (clauses cnf)

validFormula :: CNF
validFormula = Conj []

anotherUnsatFormula :: CNF
anotherUnsatFormula = Conj [Disj []]

c1 = Disj [Lit True vA, Lit True vB, Lit True vC]

c2 = Disj [Lit False vA]

c3 = Disj [Lit False vB, Lit True vC]

cnf1 = Conj [c1, c2, c3]

exampleVal2 = fromList [(Var 'A', False), (Var 'B', False), (Var 'C', True)]

testSatisfies :: Test
testSatisfies =
  "satisfies"
    ~: TestList
      [ "exampleFormula"
          ~: assertBool "" (exampleValuation `satisfies` exampleFormula),
        "another example" ~: assertBool "" (exampleVal2 `satisfies` cnf1)
      ]

---------------------------------------------------------------------------

makeValuations :: [Var] -> [Valuation]
makeValuations vs = map fromList (traverse (\x -> [(x, True), (x, False)]) vs)

-- >>> makeValuations [vA, vB, vC]
-- [fromList [(Var 'A',True),(Var 'B',True),(Var 'C',True)],fromList [(Var 'A',True),(Var 'B',True),(Var 'C',False)],fromList [(Var 'A',True),(Var 'B',False),(Var 'C',True)],fromList [(Var 'A',True),(Var 'B',False),(Var 'C',False)],fromList [(Var 'A',False),(Var 'B',True),(Var 'C',True)],fromList [(Var 'A',False),(Var 'B',True),(Var 'C',False)],fromList [(Var 'A',False),(Var 'B',False),(Var 'C',True)],fromList [(Var 'A',False),(Var 'B',False),(Var 'C',False)]]

prop_makeValuations :: CNF -> Property
prop_makeValuations p =
  length valuations === 2 ^ length ss
    .&&. allElementsDistinct valuations
  where
    valuations = makeValuations ss
    ss = vars p

allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct [] = True
allElementsDistinct (x : xs) =
  x `notElem` xs
    && allElementsDistinct xs

-- | A formula is unsatisfiable when there is no satisfying valuation
-- out of all of the possible assignments of variables to truth values
-- >>> unsatisfiable unSatFormula
-- True
unsatisfiable :: CNF -> Bool
unsatisfiable p = not . any (`satisfies` p) $ makeValuations (vars p)

testUnsatisfiable :: Test
testUnsatisfiable =
  "unsatisfiable"
    ~: TestList
      [ "unSatFormula" ~: assertBool "" (unsatisfiable unSatFormula),
        "exampleFormula" ~: assertBool "" (not (unsatisfiable exampleFormula))
      ]

---------------------------------------------------------------------------
-- Simple SAT Solver

type Solver = CNF -> Maybe Valuation

valuationsFromCNF :: CNF -> [Valuation]
valuationsFromCNF cnf = makeValuations (vars cnf)

satisfiesCNF :: CNF -> Valuation -> Bool
satisfiesCNF cnf var = satisfies var cnf

sat0 :: Solver
sat0 cnf = List.find (satisfiesCNF cnf) (valuationsFromCNF cnf)

prop_satResultSound :: Solver -> CNF -> Property
prop_satResultSound solver p = case solver p of
  Just a -> collect "sat" $ a `satisfies` p
  Nothing -> collect "unsat" $ property True

prop_satResultCorrect :: Solver -> CNF -> Property
prop_satResultCorrect solver p = property $ case solver p of
  Just a -> a `satisfies` p
  Nothing -> unsatisfiable p

---------------------------------------------------------------------------
-- Instantiation

varInClause :: Clause -> Var -> Maybe Lit
varInClause c v = List.find (\x -> var x == v) (lits c)

matchingLit :: Maybe Lit -> Bool -> Maybe Bool
matchingLit (Just l) b = Just (isPos l == b)
matchingLit Nothing b = Nothing

litInClause :: Clause -> Var -> Bool -> Maybe Bool
litInClause c v = matchingLit (varInClause c v)

removeLit :: Clause -> Lit -> Clause
removeLit c v = Disj (filter (/= v) (lits c))

instClause :: Clause -> Lit -> Maybe Clause
instClause c l = case litInClause c (var l) (polarity l) of
  Just True -> Nothing
  Just False -> Just (removeLit c (neg l))
  Nothing -> Just c

instantiate :: CNF -> Var -> Bool -> CNF
instantiate (Conj []) v b = Conj []
instantiate (Conj (x : xs)) v b = case instClause x (Lit b v) of
  Nothing -> instantiate (Conj xs) v b
  Just c -> Conj (c : clauses (instantiate (Conj xs) v b))

prop_instantiate :: CNF -> Var -> Bool
prop_instantiate s v = (Maybe.isJust (sat0 (instantiate s v True)) || Maybe.isJust (sat0 (instantiate s v False))) == Maybe.isJust (sat0 s)

falsified :: CNF -> Bool
falsified cnf = Maybe.isJust (List.find (\x -> x == Disj []) (clauses cnf))

sat1 :: Solver
sat1 = sat
  where
    sat :: CNF -> Maybe Valuation
    sat cnf 
      | satisfies emptyValuation cnf = Just emptyValuation
      | falsified cnf = Nothing
      | otherwise = 
        case vars cnf of 
          [] -> Nothing
          (x : xs) -> case sat1 (instantiate cnf x False) of
            Just y -> Just (extend x False y)
            Nothing -> 
              case sat1 (instantiate cnf x True) of
                Just y -> Just (extend x True y)
                Nothing -> Nothing                     

prop_sat1 :: CNF -> Property
prop_sat1 s = property (Maybe.isJust (sat1 s) == Maybe.isJust (sat0 s))

-- c3 = Conj {clauses = [Disj {lits = [Lit {polarity = True, var = Var 'F'}]}, Disj {lits = [Lit {polarity = False, var = Var 'F'}, Lit {polarity = True, var = Var 'F'}]}]}

-- >>> (instantiate c3 (head (vars c3)) True)
-- Conj {clauses = [Disj {lits = []}]}

--Converts AC3 Problem to CNF
convertAC3toCNF :: AC3.Cells -> [AC3.Arc] -> CNF
convertAC3toCNF = undefined


testConvert :: Test
testConvert = "Test Conversion" ~:
    solveSodukuPuzzle Soduku.arcs2x2 ~?= not (unsatisfiable (convertAC3toCNF Soduku.board2x2 Soduku.arcs2x2))

