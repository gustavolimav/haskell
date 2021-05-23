
rmPattern :: Eq w => [[w]] -> [w] -> [[w]]
rmPattern (a:as) x = elemento (intermed (a:as) x) (func(plano (intermed(a:as) x)) x)

elemento :: Eq w => [[w]] -> [w] -> [[w]]
elemento [] _ = []
elemento a [] = a
elemento (a:as) x = elemento2 a x : elemento as (revoveraq (elemento2 a x) x)

elemento2 :: Eq w => [w] -> [w] -> [w]
elemento2 [] _ = []
elemento2 _ [] = []
elemento2 (a:as) (x:xs) | a == x = a : elemento2 as xs
                        | otherwise = elemento2 as (x:xs)
                        
revoveraq :: Eq w => [w] -> [w] -> [w]
revoveraq [] a = a
revoveraq _ [] = []
revoveraq (a:as) (x:xs) | a == x = revoveraq as xs
                        | otherwise = []

plano :: Eq a => [[a]] -> [a]
plano [] = []
plano (a:as) = a ++ plano as

intermed :: Eq a => [[a]] -> [a] -> [[a]]
intermed [] _ = []
intermed (a:as) x = func a x : (intermed as x)

func :: Eq a => [a] -> [a] -> [a]
func [] _ = []
func (x:xs) (y:ys) | func2 (contardigits (y:ys)) (x:xs) == (y:ys) = func (inversofunc2 (contardigits (y:ys)) (x:xs)) (y:ys)
                   | otherwise = x : func xs (y:ys)

func2 :: Int -> [a] -> [a]
func2 0 _ = []
func2 _ [] = []
func2 i (a:as) = a : func2 (i-1) as

inversofunc2 :: Int -> [a] -> [a]
inversofunc2 0 as = as
inversofunc2 _ [] = []
inversofunc2 i (a:as) = inversofunc2 (i-1) as

contardigits :: [a] -> Int
contardigits [] = 0
contardigits (a:as) = 1 + contardigits as