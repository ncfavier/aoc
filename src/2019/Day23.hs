{-# LANGUAGE RecordWildCards #-}
module Day23 where

import AOC
import Intcode

import Data.Map (Map)
import qualified Data.Map as M

type Packet = (Integer, Integer)

data Network = Network { computers :: Map Integer Effect
                       , nat       :: Maybe Packet
                       }

getPackets :: Effect -> ([(Integer, [Integer])], Effect)
getPackets (Output a (Output x (Output y e))) = ([(a, [x, y])], e)
getPackets e = ([], e)

step :: Network -> (Maybe Packet, Network)
step Network{..} = (if isIdle then nat' else Nothing, Network computers'' nat')
    where
        (packets, computers') = first (M.fromListWith (++)) $ traverse getPackets computers
        nat' | Just (x:y:_) <- packets M.!? 255 = Just (x, y)
             | otherwise = nat
        isIdle = null packets
        distributePackets 0 | isIdle, Just (x, y) <- nat' = feed [x, y]
        distributePackets a = feed (M.findWithDefault [-1] a packets)
        computers'' = M.mapWithKey distributePackets computers'

main :: IO ()
main = do
    program <- parseInputProgram
    let Input assignAddress = runIntcode program
        network = Network (M.fromList [(i, assignAddress i) | i <- [0..49]]) Nothing
        nats = [y | Just (x, y) <- unfoldr (Just . step) network]
    print (head nats)
    print (firstDuplicate nats)
