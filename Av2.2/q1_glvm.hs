

final :: (a -> b) -> [a] -> [b]
final _ [] = []
final x (a:as) = (x a) : (final x as)

unir :: (t1 -> t2 -> t3) -> [t1] -> [t2] -> [t3]
unir _ [] [] = [] 
unir _ _ [] = []
unir _ [] _ = []
unir x (a:as) (b:bs) = (x a b) : (unir x as bs)

joinAndMap :: (t1 -> t2 -> t3) -> (t3 -> b) -> [t1] -> [t2] -> [b]
joinAndMap _ _ [] [] = []
joinAndMap x y as bs = final y (unir x as bs)




