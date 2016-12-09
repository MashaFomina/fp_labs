-- foldr
-- Правая свёртка
foldrMy                 :: (a -> b -> b) -> b -> [a] -> b
foldrMy f z []        = z
foldrMy f z (h:t)     = f h (foldrMy f z t)

-- foldl
-- Левая свёртка
foldlMy                 :: (b -> a -> b) -> b -> [a] -> b
foldlMy f z []        = z
foldlMy f z (h:t)     = foldlMy f (f z h) t

-- map
-- применить функцию к каждому элементу списка,
-- вернуть список результатов
map1                   :: (a -> b) -> [a] -> [b]
map1 f l               = foldrMy (\x zList -> (f x):zList) [] l

-- flatMap
-- применить функцию к каждому элементу списка,
-- на выходе получается набор списков, объединяем их вместе
flatMap1                 :: (a -> [b]) -> [a] -> [b]
flatMap1 f l             = foldrMy (\x zList -> (f x) ++ zList) [] l

-- concat
concat1                :: [a] -> [a] -> [a]
concat1 l1 l2          = foldrMy (\x zList -> x:zList) l2 l1

-- filter
-- выкинуть из списка все элементы,
-- не соответствующие предикату
filter1                :: (a -> Bool) -> [a] -> [a]
filter1 f l            = foldrMy (\x zList -> if (f x) then x:zList else zList) [] l

-- maxBy
maxBy                   :: (a -> Integer) -> [a] -> a
maxBy f (h:t)           = foldrMy (\x zMax -> if (f x) > (f zMax) then x else zMax) h t

-- minBy
minBy                   :: (a -> Integer) -> [a] -> a
minBy f (h:t)           = foldlMy (\zMin x -> if (f x) < (f zMin) then x else zMin) h t

-- reverse
reverse1               :: [a] -> [a]
reverse1 l             = foldlMy (\accList x -> x:accList) [] l

-- elementAt
elementAt               :: Integer -> [a] -> a
elementAt i []          = error "Empty list" -- exception
elementAt i (h:t)       = snd $ -- только значение
                            foldlMy (\(index, value) z -> if index < 1 then (index-1, value) else (index-1, z)) (i, h) t

-- indexOf
indexOf                 :: String -> [String] -> Integer
indexOf _ []            = error "Empty list" -- exception
indexOf value l         = snd $ -- только ключ
                            foldlMy (\(index, foundKey) z -> if z == value && foundKey == -1 then (index+1, index+1) else (index+1, foundKey)) (-1, -1) l