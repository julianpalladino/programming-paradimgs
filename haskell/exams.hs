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

