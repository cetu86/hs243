{-# LANGUAGE CPP #-}
import System.IO
import Data.Char (ord)
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List (intersperse,intercalate,transpose)
import System.Random (getStdRandom,randomR)
import System.Console.ANSI
 
#ifdef hs2048
dim = 5
base = 2
winat = 11
p :: Float -> Int
p x |   x < (1/7)*4 = 0
    |   x < (1/7)*6 = 1
    | otherwise = 2
#else
dim = 5
base = 3
winat = 5
p :: Float -> Int
p x |   x < 0.9 = 0
    | otherwise = 1
#endif

winAt = base^winat
r = [0..dim-1]

type Brett = [[Int]]
leeresBrett = (replicate dim . replicate dim) 0

data Achse = Hori | Verti deriving (Eq,Enum,Bounded)
data Richtung = Vor | Zurück deriving (Eq,Enum,Bounded)
type Ar = (Achse,Richtung)

neuesElement :: Brett -> IO Brett
neuesElement b = besetze <$> zufall1oder3 <*> wähle [(y,x) | x <- r, y <- r, b !! y !! x == 0]
    where besetze w' (z,s) = [if y == z then [if x == s then w' else w | (x,w) <- zip r row] 
                               else row | (y,row) <- zip r b ]
          randr n = getStdRandom (randomR  (0,n))
          wähle :: [a] -> IO a
          wähle l =  (!!) l <$> randr (length l - 1)
          zufall1oder3 :: IO Int
          zufall1oder3 = (base^) . p <$> randr 1

schritt :: Ar -> Brett -> IO Brett
schritt ar b = let b' = schiebe ar b
                in if b == b' then return b
                               else neuesElement b'

sl :: [Int] -> [Int]
sl = padd . merge . filter (/= 0) 
    where merge (x:xs) | length xs >=  bm1 && all (==x) (take bm1 xs) = x*base : merge (drop bm1 xs)
                       | otherwise = x : merge xs
          merge [] = []
          padd xs | length xs < dim = xs ++ replicate (dim - length xs) 0
                  | otherwise = xs
          bm1 = base - 1

schiebe :: Ar -> Brett -> Brett
schiebe (Verti,r)     = transpose . schiebe (Hori,r) . transpose
schiebe (Hori,Zurück) = map sl
schiebe (Hori,Vor)    = map (reverse . sl . reverse)

isOver :: Brett -> Bool
isOver b = all ( (==b) . flip schiebe b ) alleRichtungen
    where alleRichtungen :: [Ar]
          alleRichtungen = (,) <$> [minBound .. ] <*> [minBound .. ]

isWon :: Brett -> Bool
isWon = (any . elem) winAt

zeigeBrett :: Brett -> IO ()
zeigeBrett b = clearScreen >> zb' b >> putStrLn "" >> putStrLn ""
    where zb' = sequence_ . intersperse linesep . map 
                (sequence_ . intersperse colsep . map show')
          linesep = putStrLn "" >> colorStr Black (replicate (dim*(alignat+1)-1) '-') >> putStrLn ""
          colsep = colorStr Black "|"
          show' :: Int -> IO () 
          show' 0 = colorStr Black (replicate alignat ' ')
          show' x = colorStr (farbe x) $ (fillup . show) x
            where fillup x = let tofill = alignat - length x 
                              in if tofill > 0 then replicate tofill ' ' ++ x
                                               else x
          alignat = (length.show) winAt
          farbe x = let found = [c | (n,c) <- zip [0..] 
                                    [White,Cyan,Magenta,Blue,Yellow,Green],base^n == x]
                    in if null found then Black else head found
          colorStr fg str = do
              setSGR [SetColor Foreground Vivid fg, SetColor Background Dull Yellow]
              putStr str
              setSGR []

verarbeiteEingabe :: Int -> Int -> (Int,Maybe Ar)
verarbeiteEingabe 0 27 = (1,Nothing)
verarbeiteEingabe 1 91 = (2,Nothing)
verarbeiteEingabe 2 65 = (0,Just (Verti,Zurück))
verarbeiteEingabe 2 66 = (0,Just (Verti,Vor))
verarbeiteEingabe 2 67 = (0,Just (Hori,Vor))
verarbeiteEingabe 2 68 = (0,Just (Hori,Zurück))
verarbeiteEingabe _  _ = (0,Nothing)

main = do 
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    actualmain

actualmain = neuesElement leeresBrett >>= neuesElement >>= zeigeBrett `s` loop 0 
    where s f g x = f x >> g x
          loop z brett = getChar >>= flip reagiereaufEingabe brett . verarbeiteEingabe z . ord 
          reagiereaufEingabe (z,Nothing) =  loop z 
          reagiereaufEingabe (z,Just ar) =  schritt ar >=> zeigeBrett `s` behandleRandfälle z
          behandleRandfälle z brett | isOver brett = askRestart "GAME OVER!"
                                    |  isWon brett = askRestart "you win :-)"
                                    |    otherwise = loop z brett
          askRestart msg = putStr msg >> putStr " restart? (y/n)"
                              >> getconfirmation
            where getconfirmation = getChar >>= wasnun
                    where wasnun 'y' = putStrLn "" >> actualmain
                          wasnun 'n' = putStrLn "" 
                          wasnun   _ = getconfirmation
