{-Funcion: suma
    Descripcion: suma de dos numeros
    Uso: 1 + 1 es dos 
-}
suma :: Int -> Int -> Int
suma x y = x + y

{-Funcion: factorial
    Descripcion: el factorial de un numero
    Uso: factorial de 5 es 120
-}
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
