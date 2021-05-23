
remove :: Eq a => [[a]] -> [a]
remove [] = []
remove x = limpar (plano x)

plano :: Eq a => [[a]] -> [a]
plano [] = []
plano (a:as) = a ++ plano as

limpar :: Eq a => [a] -> [a]
limpar [] = []
limpar (a:as) | (contar a (a:as)) > 1 = limpar(apagar a (a:as))
              | otherwise = a : limpar as

contar :: Eq a => a -> [a] -> Int
contar _ [] = 0
contar x (a:as) | x == a = 1 + contar x as
                | otherwise = contar x as

apagar :: Eq a => a -> [a] -> [a]
apagar _ [] = []
apagar x (a:as) | x == a = as
                | otherwise = a : apagar x as
