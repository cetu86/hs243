{-# LANGUAGE RecordWildCards,NamedFieldPuns #-}
module Hs243.Console where
import Hs243.Logic
import System.IO
import System.Console.ANSI
import Control.Monad
import Data.Either
import Data.Char
import Data.List (intersperse,intercalate)

surroundWith :: a -> [a] -> [a]
surroundWith with list = (with : list) ++ [with]


zeigeBrett :: Params -> Brett -> IO ()
zeigeBrett Params {..} b = clearScreen >> header >> zb' b >> footer
    where zb' = sequence_ . intersperse linesep . map 
                (sequence_ . surroundWith colsep .intersperse colsep . map show')
          header = do 
            hDivider "┌" "┬" "┐"
            putStrLn ""
          linesep = do 
            putStrLn ""
            hDivider "├" "┼" "┤"
            putStrLn ""
          footer = do 
            putStrLn ""
            hDivider "└" "┴" "┘"
            putStrLn ""
            putStrLn ""
          hDivider a b c = colorStr Black $ a ++ intercalate b hCellDividers  ++ c
          hCellDividers = replicate dimension $ replicate alignat '─'
          colsep = colorStr Black "│"
          show' :: Int -> IO () 
          show' 0 = colorStr Black (replicate alignat ' ')
          show' x = colorStr (farbe x) $ (fillup . show) x
            where fillup x = let tofill = alignat - length x 
                              in if tofill > 0 then replicate tofill ' ' ++ x
                                               else x
          alignat = (length.show) (base^winat)
          farbe x = let found = [c | (n,c) <- zip [0..] 
                                    [White,Cyan,Magenta,Blue,Yellow,Green],base^n == x]
                    in if null found then Black else head found
          colorStr fg str = do
              setSGR [SetColor Foreground Vivid fg, SetColor Background Dull Yellow]
              putStr str
              setSGR []

data Flussbefehl = Weiter | Ende
verarbeiteEingabe :: Int -> Int -> (Int,Either Flussbefehl Ar)
verarbeiteEingabe 0 27 = (1,Left Weiter)
verarbeiteEingabe 1 91 = (2,Left Weiter)
verarbeiteEingabe 2 65 = (0,Right (Verti,Zurück))
verarbeiteEingabe 2 66 = (0,Right (Verti,Vor))
verarbeiteEingabe 2 67 = (0,Right (Hori,Vor))
verarbeiteEingabe 2 68 = (0,Right (Hori,Zurück))
verarbeiteEingabe _  n | n == ord 'q' = (0,Left Ende)
                       |    otherwise = (0,Left Weiter)

actualmain :: Config IO -> IO ()
actualmain config@(Config _ _ params) = nE (leeresBrett params ) >>= nE >>= zB `s` loop 0 
    where s f g x = f x >> g x
          zB = zeigeBrett params
          nE = neuesElement config
          loop z brett = getChar >>= flip reagiereaufEingabe brett . verarbeiteEingabe z . ord 
          reagiereaufEingabe (z,Left Weiter) =  loop z 
          reagiereaufEingabe (z,Left Ende) = const $ return ()
          reagiereaufEingabe (z,Right ar) =  schritt config ar >=> zB `s` behandleRandfälle z
          behandleRandfälle z brett | isOver params brett = askRestart "GAME OVER!"
                                    |  isWon params brett = askRestart "you win :-)"
                                    |    otherwise = loop z brett
          askRestart msg = putStr msg >> putStr " restart? (y/n)"
                              >> getconfirmation
            where getconfirmation = getChar >>= wasnun
                    where wasnun 'y' = putStrLn "" >> actualmain config
                          wasnun 'n' = putStrLn "" 
                          wasnun   _ = getconfirmation
