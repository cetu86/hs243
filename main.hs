{-# LANGUAGE CPP #-}
import System.IO
import Data.Char (ord)
import Control.Monad
import Control.Applicative
import Data.Either
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
rr = (,) <$> r <*> r

type Brett = [[Int]]
leeresBrett = (replicate dim . replicate dim) 0

data Achse = Hori | Verti deriving (Eq,Enum,Bounded)
data Richtung = Vor | Zurück deriving (Eq,Enum,Bounded)
type Ar = (Achse,Richtung)

neuesElement :: Brett -> IO Brett
neuesElement b = besetze <$> zufallszahl <*> wähle 
                        (filter ( (== 0) . ((!!) . (!!) b <$> fst <*> snd) ) rr)
    where besetze w' (z,s) = [if y == z then [if x == s then w' else w | (x,w) <- zip r row] 
                               else row | (y,row) <- zip r b ]
          randr n = getStdRandom (randomR  (0,n))
          wähle :: [a] -> IO a
          wähle l =  (!!) l <$> randr (length l - 1)
          zufallszahl :: IO Int
          zufallszahl = (base^) . p <$> randr 1

schritt :: Ar -> Brett -> IO Brett
schritt ar b = let b' = schiebe ar b
                in if b == b' then return b
                               else neuesElement b'

sl :: [Int] -> [Int]
sl = reverse . padd . foldl merge (0,0,0,[])
    where merge (p,n,l,acc) 0 = (p,n,l,acc)
          merge (p,n,l,acc) p' | p==p' && n == (base-1) 
                                    = (0,0,l+1-n,p*base : drop n acc)
                               | p==p' = (p,n+1,l+1,p:acc)
                               | otherwise = (p',1,l+1,p':acc)
          padd (_,_,l,acc) | l == dim = acc
                           | otherwise = padd (0,0,l+1,0:acc)

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

main = do 
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    actualmain

actualmain = neuesElement leeresBrett >>= neuesElement >>= zeigeBrett `s` loop 0 
    where s f g x = f x >> g x
          loop z brett = getChar >>= flip reagiereaufEingabe brett . verarbeiteEingabe z . ord 
          reagiereaufEingabe (z,Left Weiter) =  loop z 
          reagiereaufEingabe (z,Left Ende) = const $ return ()
          reagiereaufEingabe (z,Right ar) =  schritt ar >=> zeigeBrett `s` behandleRandfälle z
          behandleRandfälle z brett | isOver brett = askRestart "GAME OVER!"
                                    |  isWon brett = askRestart "you win :-)"
                                    |    otherwise = loop z brett
          askRestart msg = putStr msg >> putStr " restart? (y/n)"
                              >> getconfirmation
            where getconfirmation = getChar >>= wasnun
                    where wasnun 'y' = putStrLn "" >> actualmain
                          wasnun 'n' = putStrLn "" 
                          wasnun   _ = getconfirmation
