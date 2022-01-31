{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV
import Data.Maybe
-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct = go 1
  where
    go :: Int -> [Int] -> Int
    go acc [] = acc
    go acc (0 : _) = 0
    go acc (x : xs) = x * go acc xs

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate = concatMap (replicate 2)

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n list = (a, b ++ c)
  where
    (b, a, c) = decompose n list
      where
        decompose :: Int -> [a] -> ([a], Maybe a, [a])
        decompose _ [] = ([], Nothing, [])
        decompose ind [x]
          | ind == 0  = ([], Just x, [])
          | otherwise = ([x], Nothing, [])
        decompose ind list = go (0,[]) ind list
          where
            go :: (Int, [a]) -> Int -> [a] -> ([a], Maybe a, [a])
            go (_, ll) _ [] = (ll, Nothing, [])
            go (currInd, ll) i (x:xs)
              | currInd == i = (ll, Just x, xs)
              | otherwise    = go (currInd+1, ll ++ [x]) ind xs

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: String -> String
dropSpaces = head . words

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
    When defining the type of a treasure chest, you don't know what
    treasures it stores insight, so your chest data type must be able
    to contain any possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

newtype Gold = Gold Int
newtype XP = XP Int
newtype Health = Health Int deriving (Eq, Ord)
newtype Attack = Attack Int deriving (Eq, Ord)
newtype Endurance = Endurance Int deriving (Eq, Ord)

instance Num Health where
    (+) (Health a) (Health b) = Health (a + b)
    (*) (Health a) (Health b) = Health (a * b)
    abs (Health x) = Health (abs x)
    signum (Health x) = Health (signum x)
    (-) (Health a) (Health b) = Health (a - b)
    fromInteger = Health . fromInteger

instance Num Attack where
    (+) (Attack a) (Attack b) = Attack (a + b)
    (*) (Attack a) (Attack b) = Attack (a * b)
    abs (Attack x) = Attack (abs x)
    signum (Attack x) = Attack (signum x)
    (-) (Attack a) (Attack b) = Attack (a - b)
    fromInteger = Attack . fromInteger

instance Num Endurance where
    (+) (Endurance a) (Endurance b) = Endurance (a + b)
    (*) (Endurance a) (Endurance b) = Endurance (a * b)
    abs (Endurance x) = Endurance (abs x)
    signum (Endurance x) = Endurance (signum x)
    (-) (Endurance a) (Endurance b) = Endurance (a - b)
    fromInteger = Endurance . fromInteger

healthToInt :: Health -> Int
healthToInt (Health h) = h

attackToInt :: Attack -> Int
attackToInt (Attack atk) = atk

instance Show Gold where
    show (Gold g) = "💰 " ++ show g

instance Show XP where
    show (XP xp) = "✨ " ++ show xp

instance Show Health where
    show (Health h) = "💖 " ++ show h

instance Show Attack where
    show (Attack atk) = "🔪 " ++ show atk

instance Show Endurance where
    show (Endurance e) = "✌ " ++ show e

data Treasure = Treasure

class ChestT a where
    chestGold :: a -> Gold
    chestTreasure :: a -> Maybe Treasure

newtype ChestWithTreasureT = ChestWithTreasureT (Gold, Treasure)

instance ChestT ChestWithTreasureT where
    chestGold (ChestWithTreasureT (g, _)) = g
    chestTreasure (ChestWithTreasureT (_, t)) = Just t

newtype AcidChestT = AcidChestT Gold

instance ChestT AcidChestT where
    chestGold (AcidChestT g) = g
    chestTreasure _ = Nothing

data Chest = ChestWithTreasure ChestWithTreasureT
           | AcidChest AcidChestT

instance Show Chest where
    show chest = case chestTreasure chest of
        Just _ -> "🧰 (" ++ show (chestGold chest) ++ ", with treasure)"
        Nothing -> "🧰 (" ++ show (chestGold chest) ++ ", no treasure)"

instance ChestT Chest where
    chestGold (ChestWithTreasure cwt) = chestGold cwt
    chestGold (AcidChest ac) = chestGold ac
    chestTreasure (ChestWithTreasure cwt) = chestTreasure cwt
    chestTreasure (AcidChest ac) = chestTreasure ac

treasureChest :: Gold -> Treasure -> Chest
treasureChest g t = ChestWithTreasure (ChestWithTreasureT (g, t))

acidChest :: Gold -> Chest
acidChest g = AcidChest (AcidChestT g)

class DragonT a where
    dragonExperience :: a -> XP
    dragonFirePower :: a -> Attack
    dragonHealth :: a -> Health
    dragonSlayCounter :: a -> Int
    dragonChest :: a -> Chest

data RedDragonT = RedDragonT
    { redDragonHealth :: Health
    , redDragonFirePower :: Attack
    , redDragonSlayCounter :: Int
    , redDragonGold :: Gold
    , redDragonTreasure :: Treasure
    }

data BlackDragonT = BlackDragonT
    { blackDragonHealth :: Health
    , blackDragonFirePower :: Attack
    , blackDragonSlayCounter :: Int
    , blackDragonGold :: Gold
    , blackDragonTreasure :: Treasure
    }

data GreenDragonT = GreenDragonT
    { greenDragonHealth :: Health
    , greenDragonFirePower :: Attack
    , greenDragonSlayCounter :: Int
    , greenDragonGold :: Gold
    }

instance DragonT RedDragonT where
    dragonExperience _ = XP 100
    dragonFirePower = redDragonFirePower
    dragonHealth = redDragonHealth
    dragonSlayCounter = redDragonSlayCounter
    dragonChest d = treasureChest (redDragonGold d) (redDragonTreasure d)

instance DragonT BlackDragonT where
    dragonExperience _ = XP 150
    dragonFirePower = blackDragonFirePower
    dragonHealth = blackDragonHealth
    dragonSlayCounter = blackDragonSlayCounter
    dragonChest d = treasureChest (blackDragonGold d) (blackDragonTreasure d)

instance DragonT GreenDragonT where
    dragonExperience _ = XP 250
    dragonFirePower = greenDragonFirePower
    dragonHealth = greenDragonHealth
    dragonSlayCounter = greenDragonSlayCounter
    dragonChest = acidChest . greenDragonGold

data Dragon = RedDragon RedDragonT
            | BlackDragon BlackDragonT
            | GreenDragon GreenDragonT

instance DragonT Dragon where
    dragonExperience (RedDragon r) = dragonExperience r
    dragonExperience (BlackDragon b) = dragonExperience b
    dragonExperience (GreenDragon g) = dragonExperience g
    dragonFirePower (RedDragon r) = dragonFirePower r
    dragonFirePower (BlackDragon b) = dragonFirePower b
    dragonFirePower (GreenDragon g) = dragonFirePower g
    dragonHealth (RedDragon r) = dragonHealth r
    dragonHealth (BlackDragon b) = dragonHealth b
    dragonHealth (GreenDragon g) = dragonHealth g
    dragonSlayCounter (RedDragon r) = dragonSlayCounter r
    dragonSlayCounter (BlackDragon b) = dragonSlayCounter b
    dragonSlayCounter (GreenDragon g) = dragonSlayCounter g
    dragonChest (RedDragon r) = dragonChest r
    dragonChest (BlackDragon b) = dragonChest b
    dragonChest (GreenDragon g) = dragonChest g

withSlay :: Int -> Dragon -> Dragon
withSlay n (RedDragon r) = RedDragon (r { redDragonSlayCounter = n })
withSlay n (BlackDragon b) = BlackDragon (b { blackDragonSlayCounter = n })
withSlay n (GreenDragon g) = GreenDragon (g { greenDragonSlayCounter = n })

withHealth :: Health -> Dragon -> Dragon
withHealth h (RedDragon r) = RedDragon (r { redDragonHealth = h })
withHealth h (BlackDragon b) = BlackDragon (b { blackDragonHealth = h })
withHealth h (GreenDragon g) = GreenDragon (g { greenDragonHealth = h })

class Attackable a where
    health :: a -> Health
    dead :: a -> Bool
    withAttack :: a -> Attack -> a

instance Attackable Dragon where
    health = dragonHealth
    dead = (< 0) . health
    withAttack d atk = withSlay (dragonSlayCounter d + 1) (withHealth (Health (healthToInt (dragonHealth d) - attackToInt atk)) d)

data Knight = Knight
    { knightHealth    :: Health
    , knightAttack    :: Attack
    , knightEndurance :: Endurance
    }

instance Attackable Knight where
    health = knightHealth
    dead = (< 0) . health
    withAttack k atk = k { knightHealth = Health (healthToInt (knightHealth k) - attackToInt atk) }

data BattleOutcome = Win Chest XP
                   | Loss
                   | Fled
                   deriving (Show)

dragonFight :: Knight -> Dragon -> BattleOutcome
dragonFight k _ | dead k = Loss
dragonFight _ d | dead d = Win (dragonChest d) (dragonExperience d)
dragonFight k _ | knightEndurance k <= 0 = Fled
dragonFight k d | dragonSlayCounter d == 10 = dragonFight (withAttack k $ dragonFirePower d) (withSlay 0 d)
dragonFight k d = dragonFight (k { knightEndurance = knightEndurance k - 1 }) (withAttack (withSlay (dragonSlayCounter d + 1) d) (knightAttack k))

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing []  = True
isIncreasing [_] = True
isIncreasing (x : y : ys) = x < y && isIncreasing (y : ys)

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys)
  | y == x    = y : x: merge xs ys
  | y < x     = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort ys) (mergeSort zs)
  where
  (ys,zs)     = splitHalf xs
    where
      splitHalf :: [a] -> ([a], [a])
      splitHalf l = splitAt ((length l + 1) `div` 2) l


{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit x) = Right x
eval vars (Var v)
  | isJust it = Right (fromJust it)
  | otherwise = Left (VariableNotFound ("Error: no substution for " ++ show v))
  where it = lookup v vars
eval vars (Add exp1 exp2) = case (v1, v2) of
  (Left a, _)        -> Left (VariableNotFound ("Error: Left operand isn't a Lit," ++ show a))
  (_ , Left b)       -> Left (VariableNotFound ("Error: Right operand isn't a Lit," ++ show b))
  (Right a, Right b) -> Right (a + b)
  where
    v1 = eval vars exp1
    v2 = eval vars exp2

{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}
expandVars :: [String] -> Expr
expandVars [] = Lit 0
expandVars [a] = Var a
expandVars (x:xs) = Add (Var x) (expandVars xs)

expandIntsVars :: ([Int], [String]) -> Expr
expandIntsVars ([], b) = expandVars b
expandIntsVars ([a], b) = Add (Lit a) (expandVars b)
expandIntsVars (a, b) = Add (Lit (sum a)) (expandVars b)

flattenExpr :: Expr -> [Expr]
flattenExpr (Lit x) = [Lit x]
flattenExpr (Var x) = [Var x]
flattenExpr (Add ex1 ex2) = flattenExpr ex1 ++ flattenExpr ex2

separateFlatten :: [Expr] -> ([Int], [String])
separateFlatten l = go ([],[]) l
  where
    go :: ([Int], [String]) -> [Expr] -> ([Int], [String])
    go acc [] = acc
    go acc ((Lit a):xs) = go ([a] ++ (fst acc), snd acc) xs
    go acc ((Var a):xs) = go (fst acc, [a] ++ (snd acc)) xs
    go acc ((Add _ _):xs)  = go acc xs

constantFolding :: Expr -> Expr
constantFolding expr = expandIntsVars (separateFlatten (flattenExpr expr))
