--Plik zawierajacy funkcje sprawdzajaca warunki na zwyciestwo
--AUTOR: Filip Rutka
module Winner where

import Types 

--funkcja sprawdzajaca czy doszlo do zakonczenia rozgrywki
winner :: Pawn -> [Chequer] -> Bool
winner White board = all (== Occupied White) (take 16 board)
winner Black board = all (== Occupied Black) (take 16 (reverse board))

