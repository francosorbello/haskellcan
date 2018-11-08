import Data.Char
import Data.Array
--POR RECURSION--

--"pongomayus" pone la primera letra en mayus y el resto como minus
pongomayus (ca:co) = (toUpper ca):pongominus(co) 
--"pongominus" pone todas las letras en minus
pongominus [] = []
pongominus(ca:co) = toLower(ca):pongominus(co)

titulo2 [] = []
titulo2 (ca:co)
    |length(ca)>3 = pongomayus(ca):titulo2(co)
    |otherwise = pongominus(ca):titulo2(co)

titulo (ca:co) = pongomayus(ca):titulo2(co)
--POR COMPRENSION--

titulo_c l = [titulo2_c(x) | x<-l]

titulo2_c l
    |length(l)>3 = pongomayus(l)
    |otherwise = pongominus(l)

----B----
--POR RECURSION--
buscoletra w l pos
    |w==(l !! pos) = True
    |otherwise = False

buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama _ _ _ [] = []
buscaCrucigrama w pos len (ca:co)
    |length(ca)==len , (ca==[w]) , (buscoletra w ca pos) = ca:(buscaCrucigrama w pos len co)
    |otherwise = buscaCrucigrama w pos len co

buscaCrucigrama_c :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama_c w pos len lista = [buscoletra w x pos | x<-lista,(length x)==len]
----C----

--POR RECURSION--
posiciones l y = buscandopos l y 0
buscandopos [] _ _ = []
buscandopos (ca:co) y n
    |ca==y = n:(buscandopos co y (n+1))
    |otherwise = (buscandopos co y (n+1))

--POR COMPRENSION--
posiciones_c l y = [(x-1) | x<-[1..length(l)],(l!!(x-1) == y )] 

----F----
data Nro = Cero| Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho | Nueve deriving(Ord,Eq,Show)

full_words :: String->[Nro]

full_words l = full_words2 l 0 

full_words2 :: String->Int->[Nro]

full_words2 l x 
    |length(l) == x = []
    |(l !! x)=='0' = Cero:full_words2 l (x+1)
    |(l !! x)=='1' = Uno:full_words2 l (x+1)
    |(l !! x)=='2' = Dos:full_words2 l (x+1)
    |(l !! x)=='3' = Tres:full_words2 l (x+1)
    |(l !! x)=='4' = Cuatro:full_words2 l (x+1)
    |(l !! x)=='5' = Cinco:full_words2 l (x+1)
    |(l !! x)=='6' = Seis:full_words2 l (x+1)
    |(l !! x)=='7' = Siete:full_words2 l (x+1)
    |(l !! x)=='8' = Ocho:full_words2 l (x+1)
    |otherwise = Nueve:full_words2 l (x+1)

--POR COMPRENSION--
data Nro = Cero| Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho | Nueve deriving(Ord,Eq,Show)

full_words_c :: String -> [Nro]
full_words_c n = [full_words2_c x | x<-n]

full_words2_c :: Char -> Nro
full_words2_c x
    |x=='0' = Cero
    |x=='1' = Uno
    |x=='2' = Dos
    |x=='3' = Tres
    |x=='4' = Cuatro
    |x=='5' = Cinco
    |x=='6' = Seis
    |x=='7' = Siete
    |x=='8' = Ocho
    |otherwise = Nueve

----G----
data Notas =  ReDesaprobado | Desaprobado | Aprobado | BigAprobado

nota x
    |x<4 = ReDesaprobado
    |x<6 = Desaprobado
    |x<9 = Aprobado
    |x<11 = BigAprobado

nota2 1 = ReDesaprobado
nota2 2 = ReDesaprobado
nota2 3 = ReDesaprobado
nota2 4 = Desaprobado
nota2 5 = Desaprobado
nota2 6 = Desaprobado
nota2 7 = Aprobado
nota2 8 = Aprobado
nota2 9 = BigAprobado
nota2 10 = BigAprobado
