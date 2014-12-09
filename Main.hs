module Main where

import Data.Char

type Key = (Integer, Integer)
type Signature = (Integer, Integer)
type DSAParameter = (Integer, Integer, Integer)
type Digest = String

n :: Integer
n = 160

l :: Integer
l = 1024

--sign :: DSAParameter -> Key -> Digest -> Signature
sign (p,q,g) (x,y) msg = z
  where r = ((g^x) `mod` p) `mod` q
        z = convertS msg
        s = ()

verify :: DSAParameter -> Key -> Digest -> Signature -> Bool
verify (p,q,g) (x,y) msg sig = undefined

-- Conversion magic
convertS :: String -> Integer
convertS s = foldr (\(p,c) acc -> (+) acc . (*) (16^p). fromIntegral . digitToInt $ c) 0 s'
  where s' = zip [0..] (reverse s)

generateSecret :: DSAParameter -> Either String (Integer, Integer)
generateSecret = undefined

