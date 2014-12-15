module Main where

import Data.Char
import Numeric
import System.Exit
import System.Random
import System.IO (isEOF)
import Test.QuickCheck

type Key = (Integer, Integer)
type Signature = (Integer, Integer)
type DSAParameter = (Integer, Integer, Integer)
type Digest = String
type PublicKey = Integer 

-- Signs a message using DSA
sign :: DSAParameter -> Key -> Integer -> Digest -> Signature
sign (p,q,g) (x,y) k msg = (r, s)
  where r = (mexp p g k) `mod` q
        z = convertS msg
        kInv = k `invMod` q
        s = (kInv * (z + x*r)) `mod` q

-- Verify a DSA signature using a public key
verify :: DSAParameter -> PublicKey -> Digest -> Signature -> Bool
verify (p,q,g) y msg (r, s) = v == r
  where w = s `invMod` q
        z = convertS msg
        u1 = (z*w) `mod` q
        u2 = (r*w) `mod` q
        v = ((mexp p g u1 * mexp p y u2) `mod` p) `mod` q

-- Generates key pair x and y
generateKey :: DSAParameter -> StdGen -> (Key, StdGen)
generateKey (p,q,g) gen = ((x,y), gen')
  where (x, gen') = randomR (1, q-1) gen
        y = mexp p g x

-- Generates multiple key pairs x and y
generateKeys :: DSAParameter -> StdGen -> Integer -> [Key]
generateKeys _ _ 0 = []
generateKeys param gen count = (k:generateKeys param gen' (count-1))
  where (k,gen') = generateKey param gen

-- Generates secret short-term key k
generateSecret :: DSAParameter -> StdGen -> (Integer, StdGen)
generateSecret (_,q,_) gen = randomR (1, q-1) gen

-- Validates DSAParameter according to standard
validateDSAParameter :: DSAParameter -> StdGen -> Bool
validateDSAParameter (p,q,g) gen = and $
    [ isPrime p gen
    , isPrime q gen
    , numberOfBits p == 1024
    , numberOfBits q == 160
    , rem (p-1) q == 0
    , g > 1 && mexp p g q == 1
    ]

-- Calculate number of bits required to represent integer
numberOfBits :: Integer -> Int
numberOfBits a = length $ showIntAtBase 2 intToDigit a ""

-- Converts hexadecimal string into integer value
convertS :: String -> Integer
convertS s = foldr (\(p,c) acc -> (+) acc . fromHex p $ c) 0 s'
  where s' = zip [0..] (reverse s)
        fromHex p = (*) (16^p) . fromIntegral . digitToInt

-- Computes inverse mod b using Extended Euclidean algorithm
invMod :: Integer -> Integer -> Integer
invMod a b = i `mod` b
  where (_,i,_) = extEuclidean a b

-- Extended Euclidean algorithn
extEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extEuclidean a b
    | r == 0 = (b, 0, 1)
    | otherwise = (d, y', x' - y'*q)
 where (q, r) = quotRem a b
       (d, x', y') = extEuclidean b r

-- Computes modular exponentiation
mexp :: Integer -> Integer -> Integer -> Integer
mexp p a 0 = 1
mexp p a n
    | even n = r
    | odd n = (r*a) `mod` p
  where r = mexp p ((a*a) `mod` p) (n `div` 2)


-- Main program
main :: IO ()
main = do
    dsaParam <- readDSAParam

    gen <- newStdGen

    if validateDSAParameter dsaParam gen
        then putStrLn "valid_group"
        else putStrLn "invalid_group" >> exitFailure

    op <- getLine
    case op of
        "genkey" -> do
            n <- parseInputInteger 'n'
            printKeys $ generateKeys dsaParam gen n

        "sign"   -> do
            x <- parseInputInteger 'x'
            y <- parseInputInteger 'y'
            signMessages dsaParam (x,y) gen

        "verify" -> do
            y <- parseInputInteger 'y'
            verifyMessages dsaParam y

        _        -> exitFailure

-- Reads a named integer variable from stdin, separated by '='
parseInputInteger :: Char -> IO Integer
parseInputInteger var = parseInputString var >>= return . read

-- Reads a named string variable from stdin, separated by '='
parseInputString :: Char -> IO String
parseInputString var = do
  (var:'=':x) <- getLine
  return x
  
-- Reads DSAParameters from stdin, separated by '='
readDSAParam :: IO DSAParameter
readDSAParam = do
    p <- parseInputInteger 'p'
    q <- parseInputInteger 'q'
    g <- parseInputInteger 'g'

    return (p,q,g)

-- Reads messages from stdin and signs them, printing the signature
signMessages :: DSAParameter -> Key -> StdGen -> IO ()
signMessages (p,q,g) (x,y) gen =  do
    done <- isEOF
    if not done
        then do
            ('D':'=':d) <- getLine
            let (k, gen') = generateSecret (p,q,g) gen
            let (r,s) = sign (p,q,g) (x,y) k d
            putStrLn $ "r=" ++ show r
            putStrLn $ "s=" ++ show s
            signMessages (p,q,g) (x,y) gen'
        else return ()


-- Reads messages and signatures from stdin and verifies them
verifyMessages dsaParam y =  do
    done <- isEOF
    if not done
        then do
            d <- parseInputString 'D'
            r <- parseInputInteger 'r'
            s <- parseInputInteger 's'
            if verify dsaParam y d (r,s)
                then putStrLn "signature_valid"
                else putStrLn "signature_invalid"

            verifyMessages dsaParam y
        else return ()

-- Prints all the key formatted according to assignment
printKeys :: [Key] -> IO ()
printKeys [] = return ()
printKeys ((x,y):ks) = do
    putStrLn $ "x=" ++ show x
    putStrLn $ "y=" ++ show y
    printKeys ks


----------------- Miller-Rabin primality test -------------------

isPrime :: Integer -> StdGen -> Bool
isPrime n = probablyPrime n 100

definitelyComposite :: Integer -> Integer -> Bool
definitelyComposite a n =
    if x == 1
        then False
        else definitelyComposite' a n k x
  where (evens, odds) = span even $ iterate (`div` 2) (n-1)
        m = head odds
        k = length evens
        x = mexp n a m

definitelyComposite' _ _ 0 _ = True
definitelyComposite' a n k x = 
    if x == n - 1
        then False
        else if x' == 1
                 then True
                 else definitelyComposite' a n (k-1) x'
  where x' = mexp n x 2

probablyPrime :: Integer -> Integer -> StdGen -> Bool
probablyPrime _ 0 _ = True
probablyPrime n accuracy gen =
    if definitelyComposite a n
        then False
        else probablyPrime n (accuracy-1) gen'
  where (a, gen') = randomR (2, n-1) gen


--------------- A shitty QuickCheck test ----------------

-- Extended Euclidean Algorithm test
newtype EuclideanInput = EuclideanInput (Integer, Integer)

instance Arbitrary EuclideanInput where
    arbitrary = do
        a <- choose (1, 10000)
        b <- choose (1, a)
        return $ EuclideanInput (a,b)
  
instance Show EuclideanInput where
    show (EuclideanInput x) = show x

prop_extEuclidean :: EuclideanInput -> Property
prop_extEuclidean (EuclideanInput (a, b)) = conjoin $
    [ property $ d == gcd a b       -- correct gcd
    , d == 1 ==> gcd y a == 1       -- correct inverse (y inverse of b mod a)
    , d == 1 ==> gcd x b == 1       -- correct inverse (x inverse of a mod b)
    , property $ a*x + b*y == d     -- bezouts identity
    ]
    where (d, x, y) = extEuclidean a b
