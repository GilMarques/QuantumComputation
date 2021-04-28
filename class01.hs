module Class01 where
-- module declaration at the top

--1a)
perimeter_circle :: Floating a => a -> a
perimeter_circle r = 2*pi*r 

--1b)
area_circle :: Floating a => a -> a
area_circle r = pi*r*r

--2
factorial_ :: Int -> Int
factorial_ 0 = 1
factorial_ x = x*factorial_(x-1)

--3a)
all_numbers_upto_n :: Int -> [Int]
all_numbers_upto_n 1 = []
all_numbers_upto_n n =  all_numbers_upto_n (n-1) ++ [n]


all_numbers_upto_n2 :: Int -> [Int]
all_numbers_upto_n2 n = [2..n]


--3b)
elim_numbers :: Int -> [Int] -> [Int]
elim_numbers x [] = []
elim_numbers x (h:t) = 
    if (mod h x == 0)
        then elim_numbers x t
        else h:(elim_numbers x t)

elim_numbers2 :: Int ->[Int]->[Int]
elim_numbers2 x l = [i | i <- l,mod i x /= 0]

--3c)  
prime_list_aux :: [Int]-> [Int]
prime_list_aux [] = []
prime_list_aux (h:t) = h:prime_list_aux(elim_numbers h t)

prime_list :: Int -> [Int]
prime_list n = (prime_list_aux (all_numbers_upto_n n))


--3d)
check :: Eq a => a -> [a] -> Bool
check x [] = False
check x (h:t) = 
    if (x == h)
        then True
        else check x t



isprime :: Int -> Bool
isprime x = check x (prime_list x)


isprime2 :: Int -> Bool
isprime2 x = elem x (prime_list x)

--3e)
factorize_prime :: Int -> [Int]
factorize_prime n = [i | i<-prime_list(n), (mod n i == 0)]

--4a)
transp :: Num a => [[a]] -> [[a]]
transp [] = []
transp ([]:_) = []
transp x = map (head x):transp(map tail x)

--4b)
mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult m n = [[sum (zipWith (*) ar bc)|bc<-transp(n) ]|ar<-m]

--4c)
tensor_p :: Num a => [[a]] -> [[a]] -> [[a]]
tensor_p m n =  [[a*b | a<-rm ,b<-rn ]| rm<-m,rn<-n]

--4d)
projection :: Num a => [[a]] -> [[a]] -> [[a]]
projection m n = tensor_p m (transp n)

---5
gate ::  Num a => [[a]] -> [[a]] -> [[a]]
gate cnot v = mult cnot v
 
