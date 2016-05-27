{- ----------------------------------------------------------------------- 2016-1c-1p -}

type Nombre = String
data FileSystem = Vacio | Archivo Nombre FileSystem | Carpeta Nombre FileSystem FileSystem deriving(Show)

foldfs:: a -> (Nombre -> a -> a) -> (Nombre -> a -> a -> a) -> FileSystem -> a
foldfs fvacio farch fcarp fs = case fs of Vacio -> fvacio
                                          Archivo n fs2 -> farch n (rec fs2)
                                          Carpeta n2 fs3 fs4 -> fcarp n2 (rec fs3) (rec fs4)
                                where rec = foldfs fvacio farch fcarp

listado::FileSystem -> [Nombre]
listado = foldfs [] (:) (\n _ ls2 -> n:ls2)

rutasDeArchivo :: Nombre -> FileSystem -> [String]
rutasDeArchivo nom = foldfs [] (\n a -> if n == nom then (n:a) else a) (\n a2 a3 -> (map (\x -> (n++['/']++x)) a2) ++ a3)
