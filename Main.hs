--AUTOR: Filip Rutka
--plik zawierajacy funkcje uruchamiajaca rozgrywke
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where
import Moves
import Types
import TileChange
import Winner
import Table
import Notation

--funkcja startujaca 
main :: IO()
main =
    do
        putStrLn "Witaj w grze skoczki!!!"
        putStrLn "Wybierz kolor rozpoczynajacego skoczka (B dla bialych,C dla czarnych)"
        pionek <- getLine
        case pionek of
            "B" -> playRound White emptyboard
            "C" -> playRound Black emptyboard
            _ -> do
                putStrLn "Nieprawidlowe wejscie.Podaj jeszcze raz jaki kolor zaczyna!"
                main


playRound :: Pawn -> [Chequer] -> IO()
playRound move board =
 do
  putStrLn "Aktualny stan planszy:"
  putStr $ boardtostring board
  putStrLn "Podaj w notacji szachowej swoj ruch:"
  ruch <- getLine
  case movelenght ruch of
    False -> do
      putStrLn "Zla postac notacji, sprobuj jeszcze raz!"
      putStr $ show(length ruch)
      playRound move board
    True -> case (isnotationcorrect ruch,char3 (take 3 ruch) board move) of
      (Failure error,_) -> do
        putStrLn $ error ++ "Niepoprawna notacja, Sprobuj jeszcze raz"
        playRound move board
      (_,Fail board another) -> do
        putStrLn $ another ++ " .Sprobuj jeszcze raz"
        playRound move board
      (Correct last,Success board start) -> do
        putStr $ show(start)
        case aremovescorrect (delete3 ruch) start 0 board of
          Fail board error -> do
            putStrLn $ error ++ "Sprobuj jeszcze raz"
            playRound move board
          Success board curr -> do
            let new = pawnmove start curr board
            case winner move new of
              True -> do
                putStrLn ("ZWYCIESTWO, WYGRALY: " ++ show move)
                putStr $ boardtostring new
                return ()
              False -> playRound (next move) new


