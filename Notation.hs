--Plik zawierajacy funkcje sprawdzajace czy notacja ruchow jest poprawna
--AUTOR: Filip Rutka
module Notation where

import Data.Char (ord)
import Data.Maybe
import Types


--Funkcja zamieniajaca oznaczenie kolumny w notacji szachowej na numer tej kolumny
getBigLetterIndex :: Char -> Maybe Int
getBigLetterIndex 'A' = Just 0
getBigLetterIndex 'B' = Just 1
getBigLetterIndex 'C' = Just 2
getBigLetterIndex 'D' = Just 3
getBigLetterIndex 'E' = Just 4
getBigLetterIndex 'F' = Just 5
getBigLetterIndex 'G' = Just 6
getBigLetterIndex 'H' = Just 7
getBigLetterIndex _    = Nothing

--Funkcja zamieniajaca napis zawierajacy numer wiersza na liczbe calkowitoliczbowa
getChartoNumber :: Char -> Maybe Int
getChartoNumber x = if value < 58 && value > 47 then Just (value - 48) else Nothing where value = ord x

--Funkcja sprawdzajaca litere oraz cyfre notacji szachowej
singlenotation :: Char -> Char -> Char -> NotationFail
singlenotation a b c =
    let letter = getBigLetterIndex a
        digit = getChartoNumber b
        symbol = splitsymbol c
    in case (letter,digit,symbol) of
        (Just letter',Just digit',True) -> Correct ((8 - fromJust (Just digit'))*8 + fromJust (Just letter'))
        (_,_,_) -> Failure "Blad notacji szachowej \n"

--Funkcja sprawdzajaca zawieranie symbolu "-" miedzy przeskokami
splitsymbol :: Char -> Bool
splitsymbol x = if (x == '-') then True else False

--Funkcja sprawdzajaca czy ruch ma poprawna dlugosc
movelenght :: String -> Bool
movelenght x = if (len > 4 && len `mod` 3 == 2) then True else False where len = length x

--Funkcja zwracajaca element znajdujacy sie na okreslonym indeksie szachownicy
elementofboard :: [Chequer] -> Int -> Chequer
elementofboard [x] n = x
elementofboard (x:xs) 0 = x
elementofboard (x:xs) n = elementofboard xs (n-1)


--Funkcja sprawdzajaca czy notacja jest poprawna
isnotationcorrect :: String -> NotationFail
isnotationcorrect (a:b:c:d) =
    do  
        case singlenotation a b c of
            Correct number -> isnotationcorrect d
            Failure error -> Failure error
isnotationcorrect (a:b:[]) =
    do  
        case singlenotation a b '-' of
            Correct number -> Correct number
            Failure error -> Failure error
