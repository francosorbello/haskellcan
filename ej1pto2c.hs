
nindex l n = buscando l n 0
buscando (ca:co) n x 
        |n==x = ca
        |otherwise = buscando co n (x+1)
main=do
        print(nindex [1..5] 2)