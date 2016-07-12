main = print (sumaAlt [1..10])

{- ----------------------------------------------------------------------- ej 4 -}
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(sum2-sum1, sum1-c, c) | sum2 <- [1..], sum1 <-[1..sum2], c <-[1..sum1], (sum2-sum1)^2 + (sum1-c)^2 == c^2]


listasQueSuman :: Integer -> [[Integer]]
listasQueSuman 0 = [[]]
listasQueSuman n = [x: xs | x <- [1..n], xs <- listasQueSuman(n-x)]

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

{- ----------------------------------------------------------------------- ej 20 -}

{-data AHD tInterno tHoja = Hoja tHoja
                          | Rama tInterno (AHD tInterno tHoja)
                          | Bin (AHD tInterno tHoja) tInterno (AHD tInterno tHoja) deriving(Show)

foldAHD :: (tHoja -> b) -> (tInterno -> b -> b) -> (b -> tInterno -> b -> b) -> AHD tInterno tHoja -> b
foldAHD fHoja fRama fBin ahd = case ahd of Hoja elemHoja -> (fHoja elemHoja)
                                           Rama elemInterno ahdInterno -> (fRama elemInterno (rec ahdInterno))
                                           Bin ahdInternoL elemInterno2 ahdInternoR -> (fBin (rec ahdInternoL) elemInterno2 (rec ahdInternoR))
  where rec = foldAHD fHoja fRama fBin


mapAHD :: (a -> b) -> (c -> d) -> AHD a c -> AHD b d
mapAHD fElemInterno fElemHoja ahd = foldAHD fRecHoja fRecRama fRecBin ahd
  where fRecHoja = (\h -> Hoja (fElemHoja h))
        fRecRama = (\elemInt rec -> Rama (fElemInterno elemInt) rec)
        fRecBin = (\recL elemInt recR -> Bin recL (fElemInterno elemInt) recR)


dfsAHD :: AHD tInterno tHoja -> [tInterno]
dfsAHD ahd = foldAHD fRecHoja fRecRama fRecBin ahd
  where fRecHoja = (\h -> [])
        fRecRama = (\elemInt rec -> elemInt:rec)
        fRecBin = (\recL elemInt recR -> (elemInt:recL)++recR)

analizar :: Eq tHoja => AHD tInterno tHoja -> Either (tHoja -> Integer) [tInterno]
analizar ahd = if (equalAmountOfLeafs ahd) then (Right (dfsAHD ahd)) else (Left (countAparitionsAHD ahd))

equalAmountOfLeafs :: Eq tHoja => AHD tInterno tHoja -> Bool
equalAmountOfLeafs ahd = (foldr1 (&&) ((map (== head counts) counts)))
  where counts = [(countAparitions leafList x)  | x <- leafList]
        leafList = (leafsAHD ahd)

countAparitionsAHD :: Eq tHoja => AHD tInterno tHoja -> tHoja -> Integer
countAparitionsAHD ahd = countAparitions (leafsAHD ahd)

countAparitions :: Eq a => [a] -> a -> Integer
countAparitions xs x = foldr (\y rec -> if x == y then rec+1 else rec) 0 xs

leafsAHD :: AHD tInterno tHoja -> [tHoja]
leafsAHD ahd = foldAHD fRecHoja fRecRama fRecBin ahd
  where fRecHoja = (\h-> [h])
        fRecRama = (\_ rec -> rec)
        fRecBin = (\recL _ recR -> recL ++ recR)

-}

{- ----------------------------------------------------------------------- ej 21 -}
data AB a = Nil | Bin (AB a) a (AB a)
foldab :: b -> (b -> a -> b -> b) -> (AB a) -> b
foldab fnil fbin arbol = case arbol of Nil -> fnil
                                       Bin izq elem der -> fbin (rec izq) elem (rec der)
  where rec = foldab fnil fbin

esNil :: AB a -> Bool
esNil Nil = True
esNil (Bin _ _ _) = False

alturaab :: AB a -> Integer
alturaab = foldab 0 (\altura1 _ altura2 -> (if altura1 > altura2 then (altura1+1) else (altura2+1)))

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura


{- ----------------------------------------------------------------------- ej 22 -}
data RoseTree a = Rose a [RoseTree a] deriving(Show, Eq)

foldrt :: (a -> [b] -> b) -> (RoseTree a) -> b
foldrt frec (Rose x ls) = frec x (map (foldrt frec) ls)

roseLeafs :: Eq(a) => RoseTree a -> [a]
roseLeafs = foldrt (\x rec -> if ([] == rec) then [x] else (concat rec))

elemRose :: RoseTree a -> a
elemRose (Rose x _) = x

distancias :: RoseTree a -> RoseTree Integer
distancias = foldrt (\x rec -> if (rec == []) then Rose 0 [] else Rose 0 (map (sumarUno) rec))
  where sumarUno = foldrt (\x rec -> Rose (x+1) rec)

altura :: RoseTree a -> Integer
altura = foldrt (\_ rec -> (if (rec == []) then 0 else (maximum rec)+1))

numberOfNodes :: RoseTree a -> Integer
numberOfNodes = foldrt (\_ rec -> (sum rec) +1)

mirror :: RoseTree a -> RoseTree a
mirror = foldrt (\x rec -> Rose x (reverse rec))

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
