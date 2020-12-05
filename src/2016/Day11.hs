module Day11 where

import           Data.Set (Set)
import qualified Data.Set as Set

import AOC

data UnitType = Chip | Generator
              deriving (Eq, Ord)

data Element = Strontium | Plutonium | Thulium | Ruthenium | Curium
             deriving (Eq, Ord)

type Unit = (Element, UnitType)

data State = State { currentFloor :: Int
                   , floors :: [Set Unit]
                   } deriving (Eq, Ord)

initialState = State { currentFloor = 0
                     , floors = map Set.fromList
                         [ [(Strontium, Generator), (Strontium, Chip), (Plutonium, Generator), (Plutonium, Chip)]
                         , [(Thulium, Generator), (Ruthenium, Generator), (Ruthenium, Chip), (Curium, Generator), (Curium, Chip)]
                         , [(Thulium, Chip)]
                         , []
                         ]
                     }

check floor = null [() | (_, Generator) <- units]
            || and [(element, Generator) `Set.member` floor | (element, Chip) <- units]
            where units = Set.toList floor

isDone State{..} = currentFloor == 3 && and [null (floors !! i) | i <- [0..2]]

minSteps = head [steps | (s@State{..}, steps) <- states, isDone s] where
    states = bfs nextStates initialState
    nextStates State{..} = do
        nextFloor <- [pred currentFloor, succ currentFloor]
        guard (inRange (0, 3) nextFloor)
        size <- [1, 2]
        (moving, staying) <- pickSubset size (floors !! currentFloor)
        let newNextFloor = (floors !! nextFloor) `Set.union` moving
        guard (check staying && check newNextFloor)
        pure $ State nextFloor
                     (floors & setAt currentFloor staying
                             & setAt nextFloor newNextFloor)

main = print minSteps
