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



{- ----------------------------------------------------------------------- 2015-1c-1p -}

{- div es division entera (redondea para abajo) -}
entrelazar :: [a] -> [a] -> [a]
entrelazar lsa lsb = [if (mod i 2 == 0) then lsa!!(div i 2) else lsb!!(div i 2) | i <- [0..] ]

unos :: [Integer]
unos = [1,2,3,4,5]++unos

ceros :: [Integer]
ceros = 0:ceros

merge :: [a] -> [a] -> [a]
merge xs ys = concatMap (\(x,y) -> [x,y]) (zip xs ys)

merge2 :: [a] -> [a] -> [a]
merge2 xs ys = concat(map(\(x,y) -> [x,y]) (zip xs ys))

duplicarAparicionesL :: [a] -> [a]
duplicarAparicionesL xs = merge xs xs

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
 

prueba :: Integer -> Integer -> Integer
prueba a = head . ((\x y -> [x + y, x - y]) a)