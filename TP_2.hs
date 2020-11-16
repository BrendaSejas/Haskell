-- 1 ------------------------------------------------------------------------------------

-- suma de divisores propios de n tal que no incluya al n
sumaDeDivisoresPropios :: Int -> Int
sumaDeDivisoresPropios 0 = 0
sumaDeDivisoresPropios n = sumaDivisoresHasta n (n-1)

-- funcion auxiliar
sumaDivisoresHasta :: Int -> Int -> Int   
sumaDivisoresHasta n h | h == 0 = 0           
                       | mod n h == 0 =  h + sumaDivisoresHasta n (h-1) 
                       | otherwise = sumaDivisoresHasta n (h-1)
--

esPerfecto :: Int -> Bool
esPerfecto 1 = False
esPerfecto n = sumaDeDivisoresPropios n == n


-- 2 ------------------------------------------------------------------------------------

-- Devuelve una lista con k elementos de la sucesion alicuota generada apartir de n:
listaAlicuotaDeNLargo :: Int -> Int -> [Int]
listaAlicuotaDeNLargo 1 n = [n]
listaAlicuotaDeNLargo k n = n:(listaAlicuotaDeNLargo (k-1) (sumaDeDivisoresPropios n))


-- Decide si una lista [C1....Ck] es sociable o no.
-- condiciones:
-- debe tener distintos elementos, x = sumadeDivisoresPropios Ck , sumaDeDivisoresPropio  Ci == elemento Ci+1 y si la lista tiene un solo elemento debe ser un numero perfecto
sonSociables :: [Int] -> Bool
sonSociables (x:[]) = esPerfecto x
sonSociables (x:xs) = (sumaDeDivisoresPropios x == head xs) && not (repetidos (x:xs)) && sumaDeDivisoresPropios (ultimo (x:xs) ) == x

-- funciones auxiliares:
ultimo :: [Int] -> Int
ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs

-- decide si una lista tiene elementos repetidos
repetidos:: [Int] -> Bool
repetidos (x:[]) = False 
repetidos (x:xs) = (pertenece x xs) || repetidos xs

pertenece :: Int -> [Int] -> Bool
pertenece k [] = False
pertenece k (x:xs) = ( k == x || pertenece x xs)


-- 3 ------------------------------------------------------------------------------------

--dada una longitud k y una cota c, devuelva una lista de elementos menores o iguales a
--c tales que sean el elemento mı́nimo de un club de largo k
-- detalle devuelve la lista reves
minimosDeKClubesMenoresQue :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue k 0 = []
minimosDeKClubesMenoresQue k c | sonSociables (clubesc) = agregar (minimo clubesc) (minimosDeKClubesMenoresQue k (c-1))
                               | otherwise = minimosDeKClubesMenoresQue k (c-1)
                               where clubesc = listaAlicuotaDeNLargo k c

--

--una función tal que listaDeNClubesConNrosMenoresQue n k devuelva una lista de todos
--los clubes con n miembros cuyos elementos son todos menores que k
-- n largo  k cota
-- para pertenecer listaD.. una lista tiene que ser club y ninguno de sus elemento puede superar a k 
listaDeNClubesConNrosMenoresQue :: Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQue n 1 = []
listaDeNClubesConNrosMenoresQue n k | sonSociables (listaClub) && (maximo (listaClub)<= k ) = listaClub : (listaDeNClubesConNrosMenoresQue n (k-1))
                                    | otherwise =  listaDeNClubesConNrosMenoresQue n (k-1)
                                    where listaClub = listaAlicuotaDeNLargo n k
                                     


-- funciones auxiliares:
agregar :: Int -> [Int] -> [Int]
agregar x l | pertenece x l = l
            | otherwise = x:l

minimo :: [Int] -> Int
minimo (x:[]) = x
minimo (x:xs) | x < minimo xs = x
              | otherwise = minimo xs

maximo :: [Int] -> Int
maximo (x:[]) = x
maximo (x:xs) | x > maximo xs = x
              | otherwise = maximo xs


agregarC :: Eq a => a -> Set a -> Set a
agregarC n c | n `elem` c = c
            | otherwise = n:c

perteneceC :: Int -> [Int] -> Bool
perteneceC k [] = False
perteneceC k (x:xs) = ( k == x || pertenece x xs)

{- concatenar :: [Int] -> [[Int]]
concatenar (x:[]) = [x]:[]
concatenar (x:xs) = [x]:concatenar xs -}

-----------------------------------------------------------------------------------------
