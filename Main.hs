module Main where

import Data.Char
import System.Random

type Key = (Integer, Integer)
type Signature = (Integer, Integer)
type DSAParameter = (Integer, Integer, Integer)
type Digest = String

n :: Integer
n = 160

l :: Integer
l = 1024

sign :: DSAParameter -> Key -> Digest -> StdGen -> Integer
sign (p,q,g) (x,y) msg gen = k
  where r = ((g^x) `mod` p) `mod` q
        z = convertS msg
        (k, newGen) = random gen :: (Integer, StdGen)
        s = ()

verify :: DSAParameter -> Key -> Digest -> Signature -> Bool
verify (p,q,g) (x,y) msg sig = undefined

-- Conversion magic
convertS :: String -> Integer
convertS s = foldr (\(p,c) acc -> (+) acc . (*) (16^p). fromIntegral . digitToInt $ c) 0 s'
  where s' = zip [0..] (reverse s)

extEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extEuclidean a b
  | rem a b == 0 = (b, 0, 1)
  | otherwise = undefined

generateSecret :: DSAParameter -> Either String (Integer, Integer)
generateSecret = undefined

main :: IO ()
main = do
  gen <- newStdGen
  print $ sign (7,7,3) (10,11) "abc" gen

