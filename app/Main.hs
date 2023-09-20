data Symbol = Constant Int | Function (Int -> Int -> Int)

solveRPN :: String -> Float
solveRPN = head . foldl eval [] . words
    where
        eval (b:a:rest) "+" = (a + b):rest
        eval (b:a:rest) "-" = (a - b):rest
        eval (b:a:rest) "*" = (a * b):rest
        eval (b:a:rest) "/" = (a / b):rest
        eval (b:a:rest) "^" = (a ** b):rest
        eval (a:rest) "ln" = (log a):rest
        eval rest num = (read num):rest

main :: IO ()
main = do
    let x = [0..5]
    let y = sum (filter (>3) (map (*2) x))
    let y' = sum . filter (>3) . map (+3) . map (*2) $ x
    let y'' = sum . filter (>3) . map ((+3) . (*2)) $ x
    let message = "Hello, world!"
    let result = length $ words $ reverse message
    let result' = length . words . reverse $ message
    let another = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
    let another' = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
    print result
    print result'
