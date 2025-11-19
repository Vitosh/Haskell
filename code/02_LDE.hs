data LDE2 = LDE2 Integer Integer Integer
    deriving (Show, Eq)

type Solution2 = (Integer, Integer)

checkSolution :: Solution2 -> LDE2 -> Bool
checkSolution (x, y) (LDE2 a b c) = a * x + b * y == c

prettyPrint :: LDE2 -> String
prettyPrint (LDE2 a b c) = 
    show a ++ "*x " ++ op ++ " " ++ show (abs b) ++ "*y = " ++ show c
    where 
        op = if b < 0 then "-" else "+"

simpleProblem :: LDE2
simpleProblem = LDE2 1 4 3
shirtProblem :: LDE2
shirtProblem = LDE2 18 11 1188

extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a 0 = (a, 1, 0)
extendedEuclid a b =
    let (d, x', y') = extendedEuclid b (a `mod` b)
        x = y'
        y = x' - (a `div` b) * y'
    in (d, x, y)


concreteSolution :: LDE2 -> Maybe Solution2
concreteSolution (LDE2 a b c ) = 
    let (d, x', y') = extendedEuclid a b
    in
        if c `mod` d /= 0
            then Nothing
        else 
            let k = c `div` d -- scale factor
                x0 = x' * k
                y0 = y' * k
        in
            Just (x0, y0) 


diophantine :: LDE2 -> [(Integer, Integer)]
diophantine (LDE2 a b c) =
    case concreteSolution (LDE2 a b c) of
        Nothing -> []
        Just (x0, y0) ->
            let d     = gcd a b
                stepX =  b `div` d
                stepY =  a `div` d
                ks    = 0 : concat [[k, -k] | k <- [1..]]
            in [ (x0 + k * stepX, y0 - k * stepY) | k <- ks ]

isPositive (red, black) = red >= 0 && black >= 0
