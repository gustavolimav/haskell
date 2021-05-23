onp :: String -> String
onp [] = []
onp (')':[]) = []
onp (')':')':[]) = []
onp (x:[]) = x:[]
onp (x:')':[]) = x:[]
onp (x:y:[]) = y:x:[]
onp (x:y:xs) | elemr x ['a'..'z'] == True = (x:[]) ++ onp (y:xs)
                | x == '^' && y == '(' = onp2(x:y:xs) ++ onp(onp3(x:y:xs))
                | x == '/' && y == '(' = onp2(x:y:xs) ++ onp(onp3(x:y:xs))
                | x == '*' && y == '(' = onp2(x:y:xs) ++ onp(onp3(x:y:xs))
                | isoperator x == True && y /= '(' = (y:x:[]) ++ onp(xs)
                | isoperator x == True && y == '(' = onp (xs) ++ (x:[])
                | x == '(' = onp(y:xs)
                | x == ')' = onp(y:xs)

onp2 :: String -> String
onp2 (a:b:c:d:e:as) = (c:e:d:a:[])

onp3 :: String -> String
onp3 (a:b:c:d:e:as) = as


elemr ::(Eq a) => a -> [a] -> Bool  
elemr a [] = False  
elemr a (x:xs) | a == x = True  
               | otherwise = elemr a xs

isoperator :: Char -> Bool
isoperator y | y == '+' = True
             | y == '-' = True   
             | y == '*' = True    
             | y == '/' = True    
             | y == '^' = True 
             | otherwise = False

