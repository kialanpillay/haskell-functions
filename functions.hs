-- PAQ1
product' :: Num a => [a] -> a
product' xs = foldr (*) 1 xs

-- PAQ2
last' :: [a] -> a
last' xs = head (reverse xs)

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1)

-- PAQ3
halve :: [a] -> ([a], [a])
halve xs  = if even (length xs) then (take (length xs `div` 2) xs, drop (length xs `div` 2)  xs)
            else error "List length must be even"

-- PAQ4
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs  | null xs   = []
              | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' []      = []
safetail'' (_: xs) = xs

-- PBQ1

data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

instance Show Expr where
  show (Val n)      = show n
  show (App o x y)  = brak x ++ show o ++ brak y
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

instance Show Op where
  show Add = "+"
  show Mul = "*"

-- (App Add (Val 6) (Val 7))
-- (App Add (Val 5) (Mul (Val 2) (Val 3)))

eval :: Expr -> Int
eval (Val x)     = x
eval (App Add x y) = eval x + eval y
eval (App Mul x y) = eval x * eval y

values :: Expr -> [Int]
values (Val x)     = [x]
values (App _ x y) = values x ++ values y

-- PBQ2
delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete n (x:xs) | n == x    = xs
                | otherwise = x : delete n xs

-- PBQ3
perms :: [Int] -> [[Int]]
perms xs | null xs = [[]]
         | otherwise = concatMap (\x -> map (x:) (perms (delete x xs))) xs


permy :: [Int] -> [[Int]]
permy [] = [[]]
permy xs = [x:y | x <- xs, y <- permy (delete x xs)]

-- PBQ4
split :: [Int] -> [([Int], [Int])] 
split xs = [splitAt n xs | n <- [1..length xs - 1]]        

-- PBQ5
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [x] = [Val x]
exprs xs = [App o x y | (x, y) <- split xs, x <- exprs x, y <- exprs y, o <- [Add, Mul]]

-- PBQ6
solve :: [Int] -> Int -> [Expr]
solve xs n = [e | ps <- perms xs, e <- exprs ps, eval e == n]

bf :: [Int] -> [Int]
bf [] = []
bf [x] = [abs x]
bf (x:y:xs) =  abs x : y : bf xs

df :: [Int] -> [Int]
df [] = []
df [x] = [x + 1]
df (x:y:xs) =  (x + 1) : y : df xs

gf :: Num a => (a -> a) -> [a] -> [a]
gf f [] = []
gf f [x] = [f x]
gf f (x:y:xs) = f x : y : gf f xs

propgf :: [Int] -> Bool
propgf xs = bf xs == gf abs xs
        &&  df xs == gf (+1) xs

data Suit = Hearts | Clubs| Diamonds | Spades
    deriving Eq

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving Eq

data Card = NormalCard Rank Suit | Joker
    deriving Eq

rankCard :: Card -> Rank
rankCard (NormalCard r _) = r

countAux :: [Card] -> Int -> Int
countAux [] r = r
countAux (x:xs) r | x == Joker          = countAux xs (r + 1) 
                  | rankCard x == Ace   = countAux xs (r + 1) 
                  | otherwise           = countAux xs r

countAces :: [Card] -> Int
countAces xs = countAux xs 0

-- [(NormalCard Ace Hearts), (NormalCard Ace Spades), (NormalCard Jack Hearts), Joker, Joker, Joker]

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs)  = sort l ++ [x] ++ sort r
               where
                   l = [y | y <- xs, y <= x]
                   r = [y | y <- xs, y > x]
                
cp :: [[a]] -> [[a]]
cp [[]] = [[]] 
cp xss = sequence xss

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- (Succ(Succ(Succ Zero)))