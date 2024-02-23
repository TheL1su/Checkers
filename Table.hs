--plik zawierajacy funkcje tworzaca szachownice, oraz zamieniajacy ja na string
--AUTOR: Filip Rutka
module Table where
import Types

--Funkcja tworzaca dwa rzedy czarnych pionkow
allblack :: [Chequer]
allblack = [Occupied Black | _<-[1..16]]


--Funkcja tworzaca dwa rzedy bialych pionkow
allwhite :: [Chequer]
allwhite = [Occupied White | _<-[1..16]]

--Funkcja tworzaca rzad pol zaczynajacy sie od bialego pola
rowstartswhite :: [Chequer]
rowstartswhite = [Empty (if x `mod` 2 == 1 then W else B)| x <-[1..8]]

--Funkcja tworzaca rzad pol zaczynajacy sie od czarnego pola
rowstartsblack :: [Chequer]
rowstartsblack = [Empty (if x `mod` 2 == 1 then B else W)| x <-[1..8]]

allsquare :: [Chequer]
allsquare = rowstartswhite ++ rowstartsblack ++ rowstartswhite ++ rowstartsblack


--Funkcja tworzaca cala szachownice
emptyboard :: [Chequer]
emptyboard = allblack ++ allsquare ++ allwhite

--funkcja dzielaca tablice elementow szachownicy na wiersze i scalajaca je
rowmaker :: [Chequer] -> Int -> String
rowmaker (x:xs) n 
    | n `mod` 8 == 1 = show (((65 - n) `div` 8)) ++ " " ++ show x ++ " " ++ rowmaker xs (n+1)
    | n == 64 = show x ++ " " ++ show (1) ++ "\n"
    | n `mod` 8 == 0 = show x ++ " " ++ show (((65 - n) `div` 8) +1) ++ "\n" ++ rowmaker xs (n+1)
    | n `mod` 8 > 1 = show x ++ " " ++ rowmaker xs (n+1)

--Funckcja tworzaca napis potrzebny do wyswietlania szachownicy na wyjsciu
boardtostring :: [Chequer] -> String
boardtostring x = "  A B C D E F G H\n" ++ rowmaker x 1 ++ "  A B C D E F G H\n"


