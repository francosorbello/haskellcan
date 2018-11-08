--EJ1.2)A--

--comp x y = x>y && y+1

take_me:: Int -> [Int] -> [Int]
take_me n l = taking n l 1
taking n l c =  l

--C--

nindex l n = buscando l n 0
buscando (ca:co) n x 
        |n==x = ca
        |otherwise = buscando co n (x+1)
--B--
p :: Int -> Bool
p x = x>3

takeWhile1 :: (a->Bool)->[a]->[a]
takeWhile1 p [] = []
takeWhile1 p (ca:co)
        |p(ca)==True = ca:takeWhile1 p co
        |otherwise = []
--D--
my_elem e [] = False
my_elem e (ca:co)
    |e==ca = True
    |otherwise = my_elem e co
--H--

intercala2 x []=[[x]]
intercala2 x l@(lh:lt)=(x:l) : [lh:z|z <- intercala2 x lt]

permutaciones2 []=[[]]
permutaciones2 (lh:lt)=concat [intercala2 lh z| z<- permutaciones2 lt]

