doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:y) = (x*2) : doubleList y

maxi :: [Int] -> Int
maxi [] = 0
maxi (a:[]) = a
maxi (x:y) | x > maxi y = x
           | otherwise = maxi y

deleteN :: Int -> [Int] -> [Int]
deleteN _ [] = []
deleteN 0 y = y
deleteN i (x:y) | i == 1 = y
                | otherwise = x : deleteN (i-1) y


{-digits "ab12c3"= -"123"-
"qbc" = -""-
digits :: String -> String-}


digits :: String -> String
digits "" = ""
digits (a:as) | a `elem` ['0'..'9'] = a : digits as
              | otherwise = digits as

max' :: [Int] -> Int
max' [] = 0
max' [x] = x
max' (x:xs) | x > max' xs = x
            | otherwise = max' xs

min' :: [Int] -> Int
min' [] = 0
min' [x] = x
min' (x:xs) | x < min' xs = x
            | otherwise = min' xs


maxmin :: [Int] -> (Int, Int)
maxmin xs = (max' xs, min' xs)

--começa aq
flat :: Eq a => [[a]] -> [a]
flat [] = []
flat (a:as) = a ++ flat as

countlits :: Eq a => [a] -> [(a, Int)]
countlits [] = []
countlits (a:as) = (a, count a (a:as)) : countlits(remove a as)

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (a:as) | x == a = 1 + count x as
               | otherwise = count x as

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (a:as) | x == a = remove x as
                | otherwise = a : remove x as

group :: Eq a => [[a]] -> [(a, Int)]
group xs = countlits (flat xs)
--finaliza aq


findNlarge :: Int -> [Int] -> Int
findNlarge n xs | n == 1 = maxList xs
                | otherwise = findNlarge (n-1) (rmMax xs)

rmMax :: [Int] -> [Int]
rmMax xs = rm (maxList xs) xs

rm :: Int -> [Int] -> [Int]
rm a (x:xs) | a == x = xs
            | otherwise = x : rm a xs

maxList :: [Int] ->Int
maxList [] = 0
maxList (a:[]) = a
maxList (a:as) | a > maxList as = a
               | otherwise = maxList as

removeLargeN :: Int -> [Int] -> [Int]
removeLargeN 0 xs = xs
removeLargeN _ [] = []
removeLargeN n xs = rm (findNlarge n xs) xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap x (a:as) = (x a) : myMap x as 

operlist :: t1 -> (t1 -> t2 -> t3) -> [t2] -> [t3]
operlist _ _ [] = []
operlist n x (a:as) = (x n a) : operlist n x as

----------------------------------------------------

data Estacao = Inverno | Verao | Outono | Primavera

clima :: Estacao -> String
clima Inverno = "Frio"
clima _ = "Quente"

data Temp = Frio | Quente deriving (Show)

clima' :: Estacao -> Temp
clima' Inverno = Frio
clima' _ = Quente

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq, Ord, Enum, Bounded)
instance Show Suit where 
    show Spades = "&"
    show Hearts = "S2"
    show Diamonds = "#"
    show Clubs = "*"
-- deriving [Eq, Ord]
data Rank = Numeric Int | Jack | Queen | King | Ace
instance Show Rank where
    show (Numeric n) = show n
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

-- data Card = Card {rr::Rank, ss::Suit} deriving (Eq, Ord)
-- instance Show Card where
--     show (Card r s) = show r ++ show s
--Numeric 2 ou Numeric 10

data Part = AM | PM deriving (Eq, Show)
data Time = Local Int Int Part
          | Total Int Int

instance Eq Time where
        (==) (Local a b c) (Local d e f) = (a == d) && (b == e) && (c == f)
        (==) (Total a b) (Total c d) = (a == c) && (b == d)
        (==) (Local a b c) (Total d e) = ((partToInt a c) == d) && (b == e)
        (==) (Total d e) (Local a b c) = ((partToInt a c) == d) && (b == e)

partToInt :: Int -> Part -> Int
partToInt 12 AM = 0
partToInt 12 _ = 12
partToInt a AM = a
partToInt a PM = (a + 12)

instance Show Time where
    show (Local a b c) = show (partToInt a c) ++ ":" ++ show b
    show (Total a b) = show a ++ ":" ++ show b

instance Enum Time where
    (succ) (Local 11 59 PM) = Local 12 0 AM     
    (succ) (Local 12 59 c) = (Local 1 0 c)
    (succ) (Local 11 59 AM) = Local 12 0 PM 
    (succ) (Local a 59 c) = Local (a+1) 0 c
    (succ) (Local a b c) = Local a (b+1) c
    (succ) (Total 23 59) = Total 0 0
    (succ) (Total a 59) = Total (a+1) 0
    (succ) (Total a b) = Total a (b+1)
    (fromEnum) (Local a b c) = (partToInt a c)*60 + b
    (fromEnum) (Total a b) = a*60 + b
    (toEnum) a = Total (a `div` 60) (a `mod` 60)

data  Expr = Lit Int | Sum Expr Expr 

Sum (Sum (Lit 2) (Lit 3)) (Lit 3)

eval :: Expr -> Int
eval 