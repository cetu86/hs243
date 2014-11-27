{-# LANGUAGE RecordWildCards,NamedFieldPuns,ExistentialQuantification #-}
module Hs243.Logic where 
import Control.Monad
import Control.Applicative
import Data.List (transpose)


data Params = Params { dimension :: Int, base :: Int, winat :: Int, distribution :: [Double] }

data (Monad m, Applicative m) => Config m = Config (m Double) (Int -> m Int) Params

p :: [Double] -> Double -> Int
p d x = length (filter ( < x ) d)
 
type Brett = [[Int]]

leeresBrett Params {..} = (replicate dimension . replicate dimension) 0

data Achse = Hori | Verti deriving (Eq,Enum,Bounded,Show)
data Richtung = Vor | Zurück deriving (Eq,Enum,Bounded,Show)
type Ar = (Achse,Richtung)

neuesElement :: (Monad m, Applicative m) => Config m -> Brett -> m Brett
neuesElement (Config randr randint Params {..}) b = besetze <$> zufallszahl <*> wähle 
                        (filter ( (== 0) . ((!!) . (!!) b <$> fst <*> snd) ) rr)
    where besetze w' (z,s) = [if y == z then [if x == s then w' else w | (x,w) <- zip r row] 
                               else row | (y,row) <- zip r b ]
          wähle l =  (!!) l <$> randint (length l - 1)
          zufallszahl = (base^) . p distribution <$> randr
          r = [0..dimension-1]
          rr = (,) <$> r <*> r

schritt :: (Monad m, Applicative m) => Config m -> Ar -> Brett -> m Brett
schritt config@(Config _ _ params) ar b = let b' = schiebe params ar b
                in if b == b' then return b
                               else neuesElement config b'

sl :: Params -> [Int] -> [Int]
sl Params {..} = reverse . padd . foldl merge (0,0,0,[])
    where merge (p,n,l,acc) 0 = (p,n,l,acc)
          merge (p,n,l,acc) p' | p==p' && n == (base-1) 
                                    = (0,0,l+1-n,p*base : drop n acc)
                               | p==p' = (p,n+1,l+1,p:acc)
                               | otherwise = (p',1,l+1,p':acc)
          padd (_,_,l,acc) | l == dimension = acc
                           | otherwise = padd (0,0,l+1,0:acc)

schiebe :: Params -> Ar -> Brett -> Brett
schiebe params (Verti,r)     = transpose . schiebe params (Hori,r) . transpose
schiebe params (Hori,Zurück) = map $ sl params 
schiebe params (Hori,Vor)    = map (reverse . sl params . reverse)

isOver :: Params -> Brett -> Bool
isOver params b = all ( (==b) . flip (schiebe params ) b ) alleRichtungen
    where alleRichtungen :: [Ar]
          alleRichtungen = (,) <$> [minBound .. ] <*> [minBound .. ]

isWon :: Params -> Brett -> Bool
isWon Params {..} = (any . elem) (base^winat)

