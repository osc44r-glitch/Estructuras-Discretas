module EsParBitModulo where
import Data.Bits

{-
    Funcion: esParBit:
    Descripcion: Determina si un número entero es par evaluando su último bit en binario mediante la operación AND (.&.).
    Uso: esParBit 4 devuelve True, y esParBit 5 devuelve False.
-}

esParBit :: Int -> Bool 
esParBit n = (n .&. 1) == 0
