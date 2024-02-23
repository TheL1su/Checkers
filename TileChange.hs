--Plik zawierajacy funkcje ktore zmieniaja polozenie pionkow
--AUTOR: Filip Rutka
module TileChange where

import Types
import Table
import Notation


--Funkcja dzielaca tablice na dwie czesci; przed wstawianym elementem i za wstawianym elementem
splitboard :: Int -> Int -> ([Chequer],[Chequer],[Chequer]) -> ([Chequer],[Chequer],[Chequer])
splitboard (-1) 0 (first,second,x:xs) = (first,second,xs)
splitboard (-1) bigger (first,second,x:xs) = splitboard (-1) (bigger-1) (first,second++[x],xs)
splitboard 0 bigger (first,second,x:xs) = splitboard (-1) (bigger-1) (first,second,xs)
splitboard smaller bigger (first,second,x:xs) = splitboard (smaller-1) (bigger-1) (first++[x],second,xs)

--Funkcja zwracaja z krotki 3-elementowej, 1 element
frst (a,b,c) = a
--Funkcja zwracaja z krotki 3-elementowej, 2 element
scnd (a,b,c) = b
--Funkcja zwracaja z krotki 3-elementowej, 3 element
thrd (a,b,c) = c

--Funkcja zwracaja kwadrat ktory powinien sie pojawic po przesunieciu pionka
whichsquare :: Int -> Chequer
whichsquare n
    | even (n `div` 8) && even n = Empty W
    | (n `div` 8) `mod` 2 == 1 && (n `mod` 2 == 1) = Empty W
    | otherwise = Empty B

--Funkcja zmieniajaca polozenie pionka
pawnmove :: Int -> Int -> [Chequer] -> [Chequer]
pawnmove prev curr board = case (prev > curr) of
    True -> frst split ++ (elementofboard board prev) : scnd split ++ whichsquare prev  : thrd split where split = splitboard curr prev ([],[],board)
    False -> frst split ++ whichsquare prev : scnd split ++ (elementofboard board prev) : thrd split where split = splitboard prev curr ([],[],board)
