--sumar :: (a->Int)->[a]->a
sumar [] = 0
sumar (ca:co) = ca+(suma co)
--B--
alguno [] = False
alguno (ca:co)
    |ca==True = True
    |otherwise = alguno co
--C--
alguno [] = True
alguno (ca:co)
    |ca==True = alguno co
    |otherwise = False
--D--
--E--
restost [] _ = []
restost (ca:co) n = (ca/n):(restost co n)
--F--
incuno [] = []
incuno (ca:co) = (ca+1):incuno co
--G--
cuadrados [] = []
cuadrados (ca:co) = (ca^2):cuadrados(co)
--H--
largo l = calculo l 0
calculo [] x = x
calculo (_:co) x = calculo co (x+1)
longitudes [] = []
longitudes (ca:co) = (largo ca):longitudes co
--I--
--PAJA--

--J--
pares [] = []
pares (ca:co)
    |mod ca 2 == 0 = ca:(pares co)
    |otherwise == pares co
--K--
--COMO SE SI ES LETRA O NO?

--L--
masde [] _ = []
masde (ca:co) n
    |(largo ca)>n = ca:(masde co n)
    |otherwise = masde co n
--M--
p x = x+1
q x = x+2

cmap [] _ = []
cmap (ca:co) x = (ca x):(cmap co x)
--N--
finding _ [] x=x 
findlist sl l = finding sl l 0
finding (ca1:co1) (ca2:co2) x
    |ca1==ca2 = subfinding co1 co2
    |otherwise = finding (ca1:co1) co2 (x+1)
--subfinding verifica que encontré la sublista y no el primer elemento nada mas
subfinding _ [] = True
subfinding (ca1:co1) (ca2:co2)
    |ca1==ca2 = subfinding co1 co2
    |otherwise = False

