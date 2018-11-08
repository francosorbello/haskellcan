
findlist sl l = finding sl l 0
--finding :: [Int]->[Int]->Int->Int
finding _ [] x =(-1)
finding c1 c2@(_:co2) x
    |subfinding c1 c2 = x
    |otherwise = finding c1 co2 (x+1)
--subfinding verifica que encontré la sublista y no el primer elemento nada mas
subfinding [] _ = True
subfinding (ca1:co1) (ca2:co2)
    |ca1==ca2 = subfinding co1 co2
    |otherwise = False