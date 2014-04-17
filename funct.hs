import Data.Typeable 
import Data.Char

mygcd :: Int -> Int -> Int
mygcd aGcd bGcd
    | bGcd == 0 = aGcd
    | bGcd > 0 = mygcd bGcd (aGcd `mod` bGcd)

ispalindrome :: Eq a => [a] -> Bool
ispalindrome pally
    | length pally == 0 = False
    | length pally == 1 = True
    | length pally == 2 && (head pally == last pally) = True
    | head pally == last pally = ispalindrome (init (tail (pally)))
    | otherwise = False

intlist :: Int -> [Int]
intlist n = reverse [0..n]

quadroots :: Double -> Double -> Double -> (Double, Double)
quadroots a b c = (((-b + sqrt d) / (2 * a)),((-b - sqrt d) / (2 * a)))
     where d = b^2 - 4*a*c

rotater :: [a] -> [a]
rotater x = (last (x)):init (x)

rotateFromRight :: Int -> [a] -> [a]
rotateFromRight n x
    | n == 0 = x
    | otherwise = rotateFromRight (n-1) (rotater x) 

oddsBuilder :: [a] -> [a] -> [a]
oddsBuilder x y
    | length x == 0 = y
    | length x == 1 = (x)++y
    | otherwise = oddsBuilder (tail(tail x)) ((head (x)):(y))


odds :: [a] -> [a]
odds x = reverse (oddsBuilder (x) ([]))

