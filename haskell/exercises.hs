main = print (sumaAlt [1..10])

{- ----------------------------------------------------------------------- ej 4 -}
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(sum2-sum1, sum1-c, c) | sum2 <- [1..], sum1 <-[1..sum2], c <-[1..sum1], (sum2-sum1)^2 + (sum1-c)^2 == c^2]


{- ----------------------------------------------------------------------- ej 5 -}

primos :: [Integer] 
primos = [x | x <- [1..], primo(x)]

primo :: Integer -> Bool
primo x = [1 | y <- [2..(x-1)], mod x y == 0] == []

{- ----------------------------------------------------------------------- ej 6 -}

partir :: [a] -> [([a],[a])]
partir ls = [ (take (fromIntegral cant) ls, drop (fromIntegral cant) ls) | cant <- [0.. longitud]]
  where longitud = (toInteger (length ls))

{- ----------------------------------------------------------------------- ej 9 I -}

sumfoldr :: [Integer] -> Integer
sumfoldr = foldr (+) 0

elemfoldr :: Eq(a) => a -> [a] -> Bool
elemfoldr n = foldr (\x y -> x == n || y) False

concatfoldr :: [a] -> [a] -> [a]
concatfoldr ls1 ls2 = foldr (:) ls2 ls1

pares :: [Integer] -> [Integer]
pares = filterfoldr (\x -> mod x 2 == 0)

filterfoldr :: (a-> Bool) -> [a] -> [a]
filterfoldr f = foldr (\x y -> if (f x) then (x:y) else y) []

mapfoldr :: (a->b) -> [a] -> [b]
mapfoldr f = foldr (\x y -> (f x):y) []

{- ----------------------------------------------------------------------- ej 9 II -}

sumaAlt :: [Integer] -> Integer
sumaAlt ls = foldr (\x y -> (snd x) y (fst x)) 0 (zip ls operaciones)

operaciones :: [(Integer -> Integer -> Integer)]
operaciones = (+):((-):operaciones)

{- ----------------------------------------------------------------------- ej 9 III -}

sumaAltInverso :: [Integer] -> Integer
sumaAltInverso ls = foldr (\x y -> (snd x) y (fst x)) 0 (zip ls operaciones)

{- ----------------------------------------------------------------------- ej 22 -}
data RoseTree a = Rose a [RoseTree a] deriving(Show)

foldrt :: (a -> [b] -> b) -> (RoseTree a) -> b
foldrt frec (Rose x ls) = frec x (map (foldrt frec) ls)

roseLeafs :: Eq(a) => RoseTree a -> [a]
roseLeafs = foldrt (\x rec -> if ([] == rec) then [x] else (concat rec))

distancias :: RoseTree a -> RoseTree Integer
distancais = foldrt (\x rec -> if (rec == 0) then )


{- ----------------------------------------------------------------------- ej take -}

takefoldr :: Integer -> [Integer] -> [Integer]
takefoldr n ls = map snd (filter (\t -> fst(t) <= n) (flaguearMenores(zipearConPosiciones ls) n))

takefoldr2 :: Integer -> [(Integer,Integer)] -> [Integer]
takefoldr2 n = foldr fRec []

{-zippea con las posiciones de cada elemento-}
zipearConPosiciones :: [Integer] -> [(Integer, Integer)]
zipearConPosiciones ls = zip ([1..(toInteger (length ls))]) ls

{- para cada tupla, fst =< n   =>   fst = -1 -}
flaguearMenores :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
flaguearMenores ls n = map (reasign n) ls

{- si fst =< n   =>   fst = -1 -}
reasign :: Integer -> (Integer, Integer) -> (Integer, Integer)
reasign n tupla = if (fst tupla <= n) then (-1, snd(tupla)) else tupla

{- para cada tupla, si el first es -1, lo saca -}
fRec :: (Integer, Integer) -> [Integer] -> [Integer]
fRec tupla elem = if ((fst tupla) == -1) then (snd(tupla) : elem) else ([] ++ elem)

{- ----------------------------------------------------------------------- -}


take2 :: Integer -> [Integer] -> [Integer]
take2 n [] = []
take2 0 xs = []
take2 n (x:xs) = x:(take2 (n-1) xs)

prueba :: [(Integer, Integer)]
prueba = [(a, sum-a) | sum <- [0..], a <- [0..sum]]

listaunos :: [Integer]
listaunos = 1 : listaunos


{-

sum2 = a+b+c
sum1 = b+c

01  sum = 1, a = 0
10  sum = 1, a = 1

02  sum = 2, a = 0
11  sum = 2, a = 1
20  sum = 2, a = 2

03  
12  
21  
30  
-}
