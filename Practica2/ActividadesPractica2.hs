-- Archivo para la practica 2
{-Funcion: reconversion
    Descripción: Realiza conversion monetaria, quitando 3 ceros
    Uso: reconversion 1000 da como resultado 1.0
-}
reconversion :: Double -> Double
reconversion valor = valor / 1000


{- Funcion: cashback
    Descripción: Calcula el cashback de una tarjeta de credito equivalente al 10%
    Uso: cashback 2545 da como resultado 264
-}
cashback :: Int -> Int 
cashback montototal = montototal `div` 10



{- Funcion: cashbackMonto 
    Descripción: Obtiene el dinero equivalente a partir de puntos acumulados, multiplicando los puntos por su valor
    Uso: cashbackMonto 264 0.10 da como resultado 26.4
-}
cashbackMonto :: Double -> Double -> Double
cashbackMonto puntos valor = puntos * valor


{- Funcion: minutosHoras
    Descripción: Recibe minutos y devuelve la conversion en horas
    Uso: minutosHoras 112 da como resultado 1 hora y 52 minutos
-}
minutosHoras :: Int -> String
minutosHoras m = show (m `div` 60) ++ " horas y " ++ show (m `mod` 60) ++ " minutos "

{- Funcion: esEstafa
    Descripción: Determina si una transaccion de cambio es una estafa o no
    Uso: esEstafa 100 200 100 0 da como resultado True
-}
esEstafa :: Int -> Int -> Int -> Int -> Bool
esEstafa costo billeteGrande billeteExacto cambioDevuelto = 
    if cambioDevuelto < (billeteGrande - costo)
    then True 
    else False



{- Funcion: esDescendente
    Descripción: Corrobora si 4 numeros se ingresaron con orden descendente
    Uso: esDescendente 10 9 8 7 arroja True
-}
esDescendente :: Double -> Double -> Double -> Double -> Bool
esDescendente x y z w = x > y && y > z && z > w


{- Funcion: imc
    Descripción:recibe dos parametros, kg y cm y devuelve el imc
    Uso: imc 53.5 161 arroja normal
-}
imc :: Double -> Double -> String
imc peso altura =
    let alturaCentimetros = if altura > 3 then altura / 100 else altura
        indice = peso / (alturaCentimetros * alturaCentimetros)
    in if indice < 18.5 
       then "bajo"
       else if indice < 25.0 
            then "normal"
            else if indice < 30.0 
                 then "sobrepeso"
                 else "obesidad"


{-Funcion: hipotenusa
    Descripción: recibe dos parametros (b y h), donde b es la base y h la altura y la funcion debe devolver un valor que represente a la hipotenusa
    Uso: hipotenusa de 9.0 12.0 es 15.0
-}
hipotenusa :: Double -> Double -> Double  
hipotenusa b h = sqrt (b ^ 2 + h ^ 2)

{- Funcion: pendiente
Descripción: Esta funcion recibe dos parametros que son tuplas de dos elementos, como por ejemplo, (x1, y1) y (x2, y2). la funcion debe devolver un valor que represente la pendiente de la recta que pasa por los anteriores puntos.
Uso: pendiente de (3.0, 2.0) (7.0, 8.0) arroja 1.5
-}
pendiente :: (Double, Double) -> (Double, Double) -> Double
pendiente (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)



{- Funcion: distanciaPuntos
    Descripcion: Recibe dos parametros que seran tuplas de dos elementos de tipo flotante respectivamente, es decir, (x1, y1) y (x2, y2). La función debe devolver un valor de tipo flotante que represente la distancia entre los puntos (x1, y1) y (x2, y2).
    Uso: distanciaPuntos (2.0, 1.0) (5.0, 5.0) arroja 5.0 
-}
distanciaPuntos :: (Double, Double) -> (Double, Double) -> Double
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1) * (x2 - x1) + (y2 -y1) * (y2 - y1))


