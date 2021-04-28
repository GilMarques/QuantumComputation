module Main where

import Data.Complex
import System.Random
import Data.List

-- function from Problem Set 1
-- Matrix multiplication
gate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
gate m n = [[sum $ zipWith (*) mr nc | nc <- transpose n] | mr <- m]

-- Tensor product
tensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
tensor m n = [[ a*b | a <- mr, b <- nr] | mr <- m, nr <-n]

-- functions from Problem Set 2
s0 :: [[Complex Float]]
s0 = [[1],[0]]

s1 :: [[Complex Float]]
s1 = [[0],[1]]

had ::[[Complex Float]]
had = [[h,h],[h,-h]]
   where
      h = 1/sqrt(2)

idgate :: [[Complex Float]]
idgate = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]

cnot :: [[Complex Float]]
cnot = [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,1,0]]

s00 :: [[Complex Float]]
s00 = tensor s0 s0

bell ::[[Complex Float]]
bell = gate(gate cnot(tensor had idgate)) s00
----------------------------------------------------------
--1a
amplitudeAcc :: [[Complex Float]] -> [Float]
amplitudeAcc v = scanl1 (+) [realPart(head x)^2| x<-v]

--1b
measAcc :: [Float] -> Float -> [Int]
measAcc [] x = []
measAcc (x:xs) r
   | r>=x = 0:measAcc xs r
   | r<x = 1:replicate (length xs) 0


--1c
statetochar :: [Int] -> [Char]
statetochar l = toChar $ addLeftZeros (logBase 2.0 (myLen l)-myLen h) h
   where h = fromDecimal(getIndex 0 l)

myLen :: [Int] -> Float
myLen = foldr (\ x -> (+) 1.0) 0.0

addLeftZeros:: Float ->[Int]->[Int]
addLeftZeros 0.0 l = l
addLeftZeros x l = 0 : addLeftZeros (x-1) l

getIndex :: Int ->[Int] -> Int
getIndex x [] = 0
getIndex x (h:t)
   | h == 1 = x
   | otherwise = getIndex (x+1) t


fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = go n []
    where go 0 r = r
          go k rs = go (div k 2) (mod k 2:rs)


toChar :: [Int] -> [Char]
toChar l = [if x == 0 then '0' else '1' | x <- l]

--1d
meas ::[[Complex Float]] -> IO [Char]
meas x =  do
  n <- randomIO :: IO Float
  return $ statetochar $ measAcc (amplitudeAcc x) n

--2a
shots :: [[Complex Float]] -> Int -> IO [[Char]]
shots _ 0 = do
   return []
shots v n = do
   x<- meas v
   xs <- shots v (n-1)
   return (x:xs)



--2b
freqs ::[[Complex Float]] -> Int -> IO [([Char], Int)]
freqs v n = do
   y <- shots v n
   return (map (\x -> (head x,length x)) (group  (sort  y)))

   
main :: IO ()
main = return ()