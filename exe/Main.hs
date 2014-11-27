{-# LANGUAGE RecordWildCards,NamedFieldPuns #-}
module Main where
import System.IO
import Hs243.Console
import Hs243.Logic 
import System.Random (getStdRandom,randomR)

main = do 
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    actualmain $ Config (getStdRandom (randomR  (0,1)))
               (\n -> getStdRandom (randomR  (0,n)))
               Params { dimension = 5, 
                             base = 3, 
                            winat = 5, 
                        distribution = [0.9] }

