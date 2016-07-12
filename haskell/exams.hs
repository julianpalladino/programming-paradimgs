{- ----------------------------------------------------------------------- 2016-1c-1p -}
main = print ("holis")

type Nombre = String
data FileSystem = Vacio | Archivo Nombre FileSystem | Carpeta Nombre FileSystem FileSystem deriving(Show)

data RoseTree a = Rose a [RoseTree a] deriving(Show)

foldfs:: a -> (Nombre -> a -> a) -> (Nombre -> a -> a -> a) -> FileSystem -> a
foldfs fvacio farch fcarp fs = case fs of Vacio -> fvacio
                                          Archivo n fs2 -> farch n (rec fs2)
                                          Carpeta n2 fs3 fs4 -> fcarp n2 (rec fs3) (rec fs4)
                                where rec = foldfs fvacio farch fcarp

listado::FileSystem -> [Nombre]
listado = foldfs [] (:) (\n _ ls2 -> n:ls2)


rutasDeArchivo :: Nombre -> FileSystem -> [String]
rutasDeArchivo nom fs = map ('/':) (foldfs [] (appendIfNom nom) appendFolder fs)

appendIfNom :: Nombre -> Nombre -> [String] -> [String]
appendIfNom nom = (\n a -> if n == nom then (n:a) else a)

appendFolder :: Nombre -> [String] -> [String] -> [String]
appendFolder = (\n a2 a3 -> (map (\x -> (n++['/']++x)) a2) ++ a3)


aRoseTree :: FileSystem -> RoseTree Nombre
aRoseTree = foldfs (Rose "/" []) aRoseArchivo aRoseCarpeta

aRoseArchivo :: Nombre -> RoseTree Nombre -> RoseTree Nombre
aRoseArchivo n (Rose nRose lsRose) = Rose "/" ((Rose n []) : lsRose)

aRoseCarpeta :: Nombre -> RoseTree Nombre -> RoseTree Nombre -> RoseTree Nombre
aRoseCarpeta n (Rose nRose1 lsRose1) (Rose nRose2 lsRose2) = Rose "/" ([Rose n lsRose1]++lsRose2)


{- ----------------------------------------------------------------------- 2015-2c-1p -}

data ConcList a = Nil | Singleton a | Append (ConcList a) (ConcList a) deriving(Show)

foldCL :: b -> (a -> b) -> (b -> b -> b) -> ConcList a -> b
foldCL fDef fSing fAppend cl = case cl of Nil -> fDef
                                          Singleton x -> fSing x
                                          Append cl1 cl2 -> fAppend (rec cl1) (rec cl2)
  where rec = foldCL fDef fSing fAppend

longitud :: ConcList a -> Integer
longitud = foldCL 0 (\_ -> 1) (\rec1 rec2 -> rec1+rec2)

duplicarApariciones :: ConcList a -> ConcList a
duplicarApariciones = foldCL Nil (\x -> (Append (Singleton x) (Singleton x))) (\rec1 rec2 -> (Append rec1 rec2))

consecutivos :: [Integer]
consecutivos = concat [[n, (n+1)] | n <- [0..10]]

{- ----------------------------------------------------------------------- 2015-1c-1p -}

unos :: [Integer]
unos = [1,2,3,4,5]++unos

ceros :: [Integer]
ceros = 0:ceros

merge2 :: [a] -> [a] -> [a]
merge2 xs ys = let l = concat(map(\(x,y) -> [x,y]) (zip xs ys)) in l ++ (drop (div (length l) 2) xs) ++ (drop (div (length l) 2) ys)

entrelazar :: [a] -> [a] -> [a]
entrelazar xs ys = let l = concat(zipWith (\x y -> [x,y]) xs ys) in l ++ (drop (div (length l) 2) xs) ++ (drop (div (length l) 2) ys)

duplicarAparicionesL :: [a] -> [a]
duplicarAparicionesL xs = entrelazar xs xs

dynprog :: ([a] -> a) -> a -> Integer -> [a]
dynprog _ x 0 = [x]
dynprog f x n | n > 0 = let rec = dynprog f x (n-1) in (f rec):rec

dividirPor :: Integer -> Integer -> Integer
dividirPor d | d > 0 = head .(dynprog f 0) where
	f = \res -> if toInteger(length res) < d then 0 else 1 + (res!!fromIntegral(d-1))

factorial :: Integer -> Integer
factorial n = head (dynprog f 1 n) where
	f = (\xs -> if (toInteger (length xs)) < 2 then 1 else (toInteger(length xs)) * (head xs))

fibonacci :: Integer -> [Integer]
fibonacci n = (dynprog f 1 n) where
	f = \xs -> if ((toInteger (length xs)) < 2) then 1 else ((head xs) + (head (tail xs)))

listasQueSuman :: Integer -> [[[Integer]]]
listasQueSuman n = (dynprog f [[]] n) where
	f = \xs -> concat[map (\ys-> i:ys) (xs!!((fromIntegral i)-1)) | i <- [1..(toInteger(length xs))]]


listasQueSuman2 :: Integer -> [[Integer]]
listasQueSuman2 0 = [[]]
listasQueSuman2 n = [x: xs | x <- [1..n], xs <- listasQueSuman2(n-x)]

{-listasQueSuman :: Integer -> [[Integer]]
listasQueSuman n = (dynprog f [[]] n) where
	f = \lss -> [ [x]:xs | x <- [1..(toInteger (length lss))], xs<-lss!!((length lss)-x)] 
-}
prueba :: Integer -> Integer -> Integer
prueba a = head . ((\x y -> [x + y, x - y]) a)

{- ----------------------------------------------------------------------- 2015-1c-1r -}

data Anillo t = A t (t -> Maybe t)

fromJust :: Maybe t->t
fromJust (Just a) = a

funNaturales :: Integer -> Maybe Integer
funNaturales n | n == 0 = Just 1
			   | n == 1 = Just 2
			   | n == 2 = Just 0
			   | otherwise = Nothing

actual::Anillo t -> t
actual (A ac _) = ac

sig :: Anillo t -> t -> Maybe t
sig (A _ f) = f


ponerSig::Eq t => t -> Anillo t -> Anillo t
ponerSig elem2 (A elem f) = (A elem f2)
	where f2 n | n == elem = Just elem2
			   | n == elem2 = (f elem)
			   | otherwise = f n

avanzar :: Anillo t -> Anillo t
avanzar (A x f) = A (fromJust(f x)) f

elementos :: Eq t => Anillo t -> [t]
elementos a = (actual a) :(takeWhile (\x -> x /= (actual a)) (iterate (fromJust . sig a) (actual (avanzar a))))

preimagen :: Eq b=>(a->b)->b->[a]->Maybe a
preimagen f x = foldr (\y res -> if x == (f y) then Just y else res) Nothing



mapAnillo::Eq a => Eq b=>(a->b)->Anillo a->Anillo b
mapAnillo fMap (A x f) = A (fMap x) f2 where
	f2 m | (preimagen fMap m (elementos (A x f))) == Nothing = Nothing
		 | otherwise = (Just ((fMap.fromJust.f.fromJust) (preimagen fMap m (elementos (A x f)))))
