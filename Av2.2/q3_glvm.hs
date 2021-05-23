
aplicador :: [(a -> b -> b)] -> a -> b -> [b]
aplicador [] _ _ = []
aplicador f x y = extra (decom f x) y

decom :: [(a -> b -> b)] -> a -> [(b -> b)]
decom [] _ = []
decom (a:as) x = (a x) : decom as x

extra :: [(b -> b)] -> b -> [b]
extra [] _ = []
extra (a:as) x = (a x) : extra as x