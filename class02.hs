module Class02 where

import Data.Complex

-- Complex Float
-- 2 :+ (-1) represents 2-i  

-- function from Problem Set 1
-- Transpose 
transp :: [[Complex Float]] -> [[Complex Float]]
transp [] = []
transp ([] : _ ) = []
transp row = map head row : transp (map tail row)

-- Matrix multiplication
gate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
gate m n = [[sum $ zipWith (*) mr nc | nc <- (transp n)] | mr <- m]

-- Tensor product
tensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
tensor m n = [[ a*b | a <- mr, b <- nr] | mr <- m, nr <-n]

--------------------------------------------------------------------
--1
s0 :: [[Complex Float]]
s0 = [[1],[0]]

s1 :: [[Complex Float]]
s1 = [[0],[1]]

sh0 :: [[Complex Float]]
sh0 = gate [[1/sqrt 2,1/sqrt 2],[1/sqrt 2,-1/sqrt 2]] s0


sh1 :: [[Complex Float]]
sh1 = gate [[1/sqrt 2,1/sqrt 2],[1/sqrt 2,-1/sqrt 2]] s1
--2 a)
--00
s00 ::[[Complex Float]]
s00 = tensor s0 s0
--11
s11 :: [[Complex Float]]
s11 = tensor s1 s1

s01 ::[[Complex Float]]
s01 = tensor s0 s1
--11
s10 :: [[Complex Float]]
s10 = tensor s1 s0
--2 b)
s010 :: [[Complex Float]]
s010 = tensor s0 (tensor s1 s0)

--3 a)
-- Hint use function conjugate
-- >x = 2 :+ (-1)
-- >conjugate x
-- 2:+ 1
norm :: [[Complex Float]] -> Complex Float
norm v = sqrt (sum [head i*conjugate (head i) | i<-v ])

--3 b)
normalise :: [[Complex Float]] -> [[Complex Float]]
normalise v = [[head i/n]|i<-v]
    where n = norm v

--4 a)
--hint: recall cis 
--cis x = cos x + i sin x = e^{ix}
u3 :: (Float, Float, Float) -> [[Complex Float]]
u3 (t, p, l) = [[cos (t/2):+ 0.0,cis p*(sin(t/2):+0.0)],[-cis(l)*(sin(t/2):+0.0),cis(p+l)*(cos(t/2):+0.0)]]

--4 b)
-- Gate Z
au3 :: [[Complex Float]]
au3 = u3 (0,pi,0)
--
--Gate H
bu3 :: [[Complex Float]]
bu3 = u3 (pi/2,0,pi)
--

--5 a)
systa :: [[Complex Float]]
systa = gate [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,1,0]] (gate (tensor [[h,h],[h,-h]] [[1:+0,0:+0],[0:+0,1:+0]]) s00) 
    where h = 1/sqrt 2
--5 b)
-- 00 | 00
-- 01 | 11
-- 10 | 10
-- 11 | 01
systb ::[[Complex Float]]
systb = gate (tensor [[h,h],[h,-h]] [[h,h],[h,-h]]) (gate [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,1,0]] (gate (tensor [[h,h],[h,-h]] [[h,h],[h,-h]]) s00))
    where h = 1/sqrt 2


--6
iden :: Int -> Int -> Complex Float
iden i j = if i==j then 1.0:+0.0 else 0.0:+0.0 

identity :: Int -> [[Complex Float]]
identity n = [ [iden i j| i <- [1..n]] | j <- [1..n]]

px :: Int -> [[Complex Float]]
px 2 = [[0,1],[1,0]]
px n = tensor [[1,0],[0,1]] (px (n-1))


cnot_ :: Int -> [[Complex Float]]
cnot_ n = zipWith (zipWith (+)) (tensor [[1,0],[0,0]] (identity (2^(n-1)))) (tensor  [[0,0],[0,1]] (px n))
