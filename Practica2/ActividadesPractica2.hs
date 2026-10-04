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
