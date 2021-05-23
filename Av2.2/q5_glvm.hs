checkSudoku :: [[Int]] -> Bool
checkSudoku x | (linhascheck x) && (colunascheck x) && (macheck x) && (checkUmNove x) = True
              | otherwise = False

--UmNove
checkUmNove :: [[Int]] -> Bool
checkUmNove [] = True
checkUmNove (a:as) | numUmNove a == True = checkUmNove as
                   | otherwise = False

numUmNove :: [Int] -> Bool
numUmNove [] = True
numUmNove (a:as) | a < 10 = numUmNove as
                 | otherwise = False
--Linhas
linhascheck :: [[Int]] -> Bool
linhascheck [] = True
linhascheck (a:as) | repeticao a == True = colunascheck as
                   | otherwise = False

repeticao :: [Int] -> Bool
repeticao [] = True
repeticao (a:as) | elemr a as == True = repeticao as
                 | otherwise = False

elemr ::Int -> [Int] -> Bool  
elemr a [] = True  
elemr a (x:xs) | a == x = False  
               | otherwise = elemr a xs

--colunas
colunascheck :: [[Int]] -> Bool
colunascheck [] = True
colunascheck (a:as) | repeticao (selecionarLista(criarcolunas (a:as))) == True = colunascheck as
                    | otherwise = False

criarcolunas :: [[Int]] -> [[Int]]
criarcolunas (a:as) | a == [] = []
                    | otherwise = colunaN (a:as) : criarcolunas(removerPrimeiro (a:as))

colunaN :: [[Int]] -> [Int]
colunaN [] = []
colunaN (a:as) = colunaprimeiro a : colunaN as

removerPrimeiro :: [[Int]] -> [[Int]]
removerPrimeiro [] = []
removerPrimeiro (a:as) | a /= [] = removerPrimeiro2 a : removerPrimeiro as
                       | otherwise = removerPrimeiro as

removerPrimeiro2 :: [Int] -> [Int]
removerPrimeiro2 [] = []
removerPrimeiro2 (a:as) = as

colunaprimeiro :: [Int] -> Int
colunaprimeiro (a:as) = a

selecionarLista :: [[Int]] -> [Int]
selecionarLista [] = []
selecionarLista (a:as) = a
--Matrix

macheck :: [[Int]] -> Bool
macheck (a:as) | repeticao((primeiroq 3 (a:as))) &&  repeticao(primeiroq 3 (removertreslistas 3 (a:as))) && repeticao(primeiroq 3 (removertreslistas 6 (a:as))) && repeticao(primeiroq 3 (removertresprimeiros 3 (a:as))) && repeticao(primeiroq 3 (removertreslistas 3 (removertresprimeiros 3 (a:as)))) && repeticao(primeiroq 3 (removertreslistas 6 (removertresprimeiros 3 (a:as)))) && repeticao(primeiroq 3 (removertresprimeiros 6 (a:as))) && repeticao(primeiroq 3 (removertreslistas 3 (removertresprimeiros 6 (a:as)))) && repeticao(primeiroq 3 (removertreslistas 6 (removertresprimeiros 6 (a:as)))) = True 
                | otherwise = False


removertreslistas :: Int -> [[Int]] -> [[Int]]
removertreslistas 0 x = x 
removertreslistas i (a:as) = removertreslistas (i-1) (as)

removertresprimeiros :: Int -> [[Int]] -> [[Int]]
removertresprimeiros i [] = []
removertresprimeiros i (a:as) | a /= [] = (removertresprimeiros2 i a) : removertresprimeiros i as
                       | otherwise = removertresprimeiros i as

removertresprimeiros2 :: Int -> [Int] -> [Int]
removertresprimeiros2 0 as = as  
removertresprimeiros2 i (a:as) = removertresprimeiros2 (i-1) as

primeiroq :: Int -> [[Int]] -> [Int]
primeiroq 0 _ = []
primeiroq i (a:as) = (listatres 3 a) ++ primeiroq (i-1) as

listatres :: Int -> [Int] -> [Int]
listatres _ [] = []
listatres 0 _ = []
listatres i (a:as) = a : listatres (i-1) as
------------------------------------------------------------------------