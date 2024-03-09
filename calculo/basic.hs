derivada_simples :: [(Int, Int)] -> [(Int, Int)]
derivada_simples expressao 
    | length expressao == 0 = []
    | otherwise = [(coeficiente*potencia, potencia-1)] ++ derivada_simples (tail expressao)
    where 
        (coeficiente, potencia) = head expressao

integral_simples :: [(Float, Float)] -> [(Float, Float)]
integral_simples expressao
    | length expressao == 0 = []
    | otherwise = [(coeficiente / potencia, potencia+1)] ++ integral_simples (tail expressao)
    where
        (coeficiente, potencia) = head expressao

