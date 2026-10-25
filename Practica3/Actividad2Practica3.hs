data Plazo = Dias Float | Meses Float deriving Show
--Defino mi tipo de dato para el plazo

{-
    Funcion: cetes
    Descripcion: Calcula el valor final de una inversion en Cetes 
    Uso: cetes 100 (Dias 28) da como resultado 100.47366
-}

cetes :: Float -> Plazo -> Float
cetes vi (Dias 28) = vi * (1 + 0.0609 * (28 / 360))
cetes vi (Meses 1) = vi * (1 + 0.0609 * (28 / 360))

cetes vi (Dias 182) = vi * (1 + 0.0691 * (182 / 360))
cetes vi (Meses 6) = vi * (1 + 0.0691 * (182 / 360))

cetes vi (Dias 364) = vi * (1 + 0.0744 * (364 / 360))
cetes vi (Meses 12) = vi * (1 + 0.0744 * (364 / 360))

cetes vi _   = vi
-- Caso si se ingresa un valor diferente a los establecidos 


{-
    Funcion: mensajesCetes
    Descripcion: recibe un monto inicial de inversión, un plazo expresado en días o meses y devuelve un mensaje que indique: El plazo de la inversión, el rendimiento bruto obtenido y el monto acumulado al finalizar el plazo.
    Uso: mensajesCetes 100 (Dias 28) nos arroja: Despues de 28.0 dias, obtuvo 0.47366333de rendimiento bruto sin asumir algun riesgo crediticio. El monto total de la inversion es de 100.47366.
-}

mensajesCetes :: Float -> Plazo -> String
mensajesCetes vi (Dias n) =  
    "Despues de " ++ show n ++ " dias, obtuvo " ++ show m ++ "de rendimiento bruto sin asumir algun riesgo crediticio. El monto total de la inversion es de " ++ show k ++ "."
    where 
    k = cetes vi (Dias n)
    m = k - vi


{-
    Funcion: negativos
    Descripcion: cuenta los números negativos de una lista.
    Uso: negativos [-1, 2, 4, 6, -4] arroja 2
-}

negativos :: [Int] -> Int
negativos xs = length [x | x <- xs, x < 0]
