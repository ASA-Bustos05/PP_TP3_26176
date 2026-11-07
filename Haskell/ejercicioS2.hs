-- 1. POTENCIA (Esquema de patrones)
potencia :: Int -> Int -> Int
potencia base 0 = 1
potencia base exponente = base * potencia base (exponente - 1)

-- 2. VALOR ABSOLUTO (Con Guardas)
valAbsoluto :: Int -> Int
valAbsoluto x
    | x < 0     = -x
    | otherwise = x
    
-- 3. VALOR ABSOLUTO A (Con If-Then-Else)
valAbsolutoA :: Int -> Int
valAbsolutoA x = if x < 0 then -x else x

-- 4. SUMATORIA
sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n - 1)

-- 5. DIVISOR A (Con If-Then-Else corregido)
divisorA :: Int -> Int -> Bool
divisorA a 0 = a > 0  
divisorA a b =
    if b < 0 then 
        divisorA a (-b)
    else if a <= 0 then 
        False
    else if b < a then 
        False
    else 
        divisorA a (b - a)

-- 6. DIVISOR (Con Guardas)
divisor :: Int -> Int -> Bool
divisor a b
    | b < 0     = divisor a (-b)  
    | a <= 0    = False           
    | b == 0    = True            
    | b < a     = False           
    | otherwise = divisor a (b - a) 

-- 7. MÁXIMO COMÚN DIVISOR
mcd :: Int -> Int -> Int
mcd a b
    | b == 0    = abs a          
    | otherwise = mcd b (mod a b)

