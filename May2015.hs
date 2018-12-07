module May2015 where

import Prelude hiding (and, reverse, replicate, filter)

-- Question 2
-- a) Recursion

triangle :: Int -> Int
triangle 0 = 0
triangle n = n + triangle (n-1)

-- b) Higher-order library function

triangle' :: Int -> Int
triangle' n = foldr (+) 0 [0..n]

-- c) List comprehension

triangle'' :: Int -> Int
triangle'' n = sum [x | x <- [0..n]]

-- d) Recursions

and :: [Bool] -> Bool
and []     = True
and (x:xs) = if x == False then False else and xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x == True   = x : filter f xs
    | otherwise     = filter f xs

-- Question 3
-- a)

insert :: Int -> [Int] -> [Int]
insert x []     = [x]
insert x (y:ys)
    | x <= y    = x : (y:ys)
    | otherwise = y : insert x ys

-- b)

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- c)

smaller :: Int -> [Int] -> [Int]
smaller x ys = [y | y <- ys, y < x]

larger :: Int -> [Int] -> [Int]
larger x ys = [y | y <- ys, y > x]

-- d)

qsort :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) = qsort (smaller x xs)
    ++ [x]
    ++ qsort (larger x xs)

-- Question 4

data Tree = Leaf Int | Node Tree Tree
    deriving Show

-- a)

t1 :: Tree
t1 = Leaf 1

t2 :: Tree
t2 = Node (Leaf 1) (Leaf 2)

t3 :: Tree
t3 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

-- b)

leaves :: Tree -> [Int]
leaves (Leaf x)   = [x]
leaves (Node l r) = (leaves l) ++ (leaves r)

size :: Tree -> Int
size (Leaf x)   = 1
size (Node l r) = size l + size r

-- c)

balanced :: Tree -> Bool
balanced (Leaf x)   = True
balanced (Node l r) = size l == size r

-- d)

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where
    n = (length xs) `div` 2

-- e)

balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs = Node (balance l) (balance r) where
    (l, r) = halve xs

-- Question 5

data Prop =
      X
    | F
    | T
    | Not Prop
    | And Prop Prop 
    deriving Show

-- a)

eval :: Prop -> Bool -> Bool
eval X v           = v
eval F _           = False
eval T _           = True
eval (Not p) v     = not (eval p v)
eval (And p0 p1) v = (eval p0 v) && (eval p1 v)

-- b)

isEq :: Prop -> Prop -> Bool
isEq X X                     = True
isEq F F                     = True
isEq T T                     = True
isEq (Not x) (Not y)         = isEq x y
isEq (And x0 x1) (And y0 y1) = (isEq x0 y0) && (isEq x1 y1)
isEq _ _                     = False

-- simplify :: Prop -> Prop
-- simplify (Not T)       = F
-- simplify (Not F)       = T
-- simplify (Not (Not p)) = simplify p  -- Corrected
-- simplify (And T p)     = simplify p  -- Corrected
-- simplify (And F _)     = F
-- simplify (And p T)     = simplify p  -- Corrected
-- simplify (And _ F)     = F
-- simplify (And x y)
--     | isEq x y  = simplify x  -- Corrected
--     | otherwise = And (simplify x) (simplify y)  -- Corrected
-- simplify p = p

simplify :: Prop -> Prop
simplify X = X
simplify F = F
simplify T = T
simplify (Not p) = let sp = simplify p in
    case sp of
        T        -> F
        F        -> T
        (Not p') -> p'
        _        -> Not sp
simplify (And x y) = 
    let sx = simplify x
        sy = simplify y
    in case sx of T -> sy
                  F -> F
                  _ -> case sy of T -> sx
                                  F -> F
                                  _ -> if isEq sx sy then sx else And sx sy

