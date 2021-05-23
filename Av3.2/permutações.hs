permute :: (Ord a) => [a] -> [[a]]
permute a = insertionSort(removerrepeticao(permute1 a))

permute1 :: (Eq a) => [a] -> [[a]]
permute1 [ ] = [[ ]++[ ]]
permute1 ( x : xs ) = (foldin (++) ([ ]++[ ]) ( mapin ( permute2 ([ ]++[ ]) x ) ( permute1 xs) ) )

permute2 :: (Eq a) => [a] -> a -> [a] -> [[a]]
permute2 xs x [] = [ xs ++ (x : []) ]
permute2 xs x ( y:ys ) = ( xs ++ ( x:y:ys ) ) : ( permute2 ( xs ++ (y:[]) ) x ys)
---------------------------------------------------------------------------------------
elemr ::(Eq a) => a -> [a] -> Bool  
elemr a [] = True
elemr a (x:xs) | a == x = False  
               | otherwise = elemr a xs

mapin :: (Eq a) => (a -> b) -> [a] -> [b]
mapin _ [] = []
mapin a (x:xs) = a x : mapin a xs

foldin :: (Eq a) => (a->b->b) -> b -> [a] -> b
foldin _ z [] = z
foldin fun z (x:xs) = fun x (foldin fun z xs)
---------------------------------------------------------------------------------------
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertaux x (insertionSort xs)

insertaux :: (Ord a) => a -> [a] -> [a]
insertaux x [] = (x:[])
insertaux x (y:ys) | x <= y = (x:y:ys)
                   | otherwise = y: (insertaux x ys)
---------------------------------------------------------------------------------------
removerrepeticao :: Eq a => [a] -> [a]
removerrepeticao [] = []
removerrepeticao (a:as) | (elemr a as) == False = removerrepeticao as
                        | otherwise = a : removerrepeticao as