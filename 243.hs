import System.IO
import Data.Char (ord)
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List (intersperse,intercalate,transpose)
import System.Random (getStdRandom,randomR)
import System.Console.ANSI
 
dim = 5
base = 3
r = [0..dim-1]

type Brett = [[Int]]
leeresBrett = replicate dim $ replicate dim 0

data Achse = Hori | Verti deriving (Eq,Show,Enum,Ord,Bounded)
data Richtung = Vor | Zurück deriving (Eq,Show,Enum,Ord,Bounded)

neuesElement :: Brett -> IO Brett
neuesElement b = do
                    (z,s) <- wähle [(y,x) | x <- r, y <- r, b !! y !! x == 0]
                    w' <- zufall1oder3
                    return [if y == z then [if x == s then w' else w | (x,w) <- zip r row] 
                               else row | (y,row) <- zip r b ]
    where wähle :: [a] -> IO a
          wähle l =  (!!) l <$> getStdRandom (randomR  (0,length l - 1))
          zufall1oder3 :: IO Int
          zufall1oder3 = (base^) . p <$> getStdRandom (randomR  (0,1))
              where p :: Float -> Int
                    p x |   x < 0.9 = 0
                        | otherwise = 1

schritt :: Brett -> Achse -> Richtung -> IO Brett
schritt b a r = let b' = schiebe b a r
                 in if b == b' then return b
                               else neuesElement b'

sl :: [Int] -> [Int]
sl = padd . merge . filter (/= 0) 
    where merge (x1:x2:x3:xs) | x1 == x2 && x1 == x3 = x1+x2+x3 : xs
                              | otherwise = x1 : merge (x2:x3:xs)
          merge (x:xs) = x : merge xs
          merge [] = []
          padd xs | length xs < dim = xs ++ replicate (dim - length xs) 0
                  | otherwise = xs

schiebe :: Brett -> Achse -> Richtung -> Brett
schiebe b Verti r = transpose $ schiebe (transpose b) Hori r
schiebe b Hori Zurück = map sl b
schiebe b Hori Vor    = map (reverse . sl . reverse) b

colorStr :: Color -> String -> IO ()
colorStr fg str = do
  setSGR [SetColor Foreground Vivid fg, SetColor Background Dull Yellow]
  putStr str
  setSGR []

zeigeBrett :: Brett -> IO ()
zeigeBrett b = clearScreen >> sequence (intersperse linesep (map (sequence_ . intersperse colsep . map show') b)) >> 
                putStrLn "" >> putStrLn ""
    where linesep = putStrLn "" >> (colorStr Black $ replicate (dim*4-1) '-') >> putStrLn ""
          colsep = colorStr Black "|"
          show' :: Int -> IO () 
          show' 0 = colorStr Black "   "
          show' x = colorStr (fcode x) $ (fillup . show) x
            where fillup x = let tofill = 3 - length x 
                              in if tofill > 0 then replicate tofill ' ' ++ x
                                               else x
          fcode 1 = White
          fcode 3 = Cyan
          fcode 9 = Magenta
          fcode 27 = Blue
          fcode 81 = Yellow
          fcode 243 = Green
          fcode _ = Black

verarbeiteEingabe :: Int -> Int -> (Int,Maybe (Achse,Richtung))
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
    brett <- neuesElement leeresBrett >>= neuesElement
    zeigeBrett brett
    loop 0 brett
    where loop zustand brett = do
            c <- getChar
            let (neuerZustand,vltRichtung) = verarbeiteEingabe zustand (ord c)
            case vltRichtung of Nothing -> loop neuerZustand brett
                                Just (a,r) -> do
                                            brett' <- schritt brett a r
                                            zeigeBrett brett'
                                            if all (\(a,r) -> schiebe brett' a r == brett' ) ((,) <$> 
                                                    [minBound :: Achse .. ] <*> [minBound :: Richtung .. ])
                                                then putStrLn "GAME OVER!"
                                                else if any (elem 243) brett' then putStrLn "you win :-)"
                                                                              else loop neuerZustand brett'
