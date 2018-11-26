data Op = Add | Sub | Mul | Div

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0

data Expr = Val Int | App Op Expr Expr

eval :: Expr -> [Int]
eval (Val n)        = [n]
eval (App op e1 e2) = [apply op n1 n2 | n1 <- eval e1, n2 <- eval e2, valid op n1 n2]

-- Code will be released
choices :: [a] -> [[a]]
choices = undefined

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
    elem (values e) (choices ns) && eval e == [n]

-- Code will be released
split :: [a] -> [([a], [a])]
split = undefined

-- Key function in the lecture
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- combine {2+3} {7*6} = [{2+3+7*6}, {2+3-7*6}, {2+3*7*6}, {2+3/7*6}]
combine :: Expr -> Expr -> [Expr]
combine l r = [App op l r | op <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | c <- choices ns, e <- exprs c, eval e == [n]]

----- Improvement-----

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [e | (ls, rs) <- split ns, lx <- results ls, rx <- results rs, e <- combine' lx rx]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App op l r, apply op x y) | op <- ops, valid op x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

----- Improvement -----

-- Exploiting numeric properties
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0
