module Day11 where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Lens.Micro.Platform

import AOC

data UnitType = Chip | Generator
              deriving (Eq, Ord, Show)

data Element = Strontium | Plutonium | Thulium | Ruthenium | Curium | Elerium | Dilithium
             deriving (Eq, Ord, Show)

type Unit a = (a, UnitType)

data State a = State { _currentFloor :: Int
                     , _floors :: Vector (Set (Unit a))
                     } deriving (Eq, Ord, Show)

makeLenses ''State

initialState = State { _currentFloor = 0
                     , _floors = Vec.fromList $ map Set.fromList
                         [ [(Strontium, Generator), (Strontium, Chip), (Plutonium, Generator), (Plutonium, Chip)]
                         , [(Thulium, Generator), (Ruthenium, Generator), (Ruthenium, Chip), (Curium, Generator), (Curium, Chip)]
                         , [(Thulium, Chip)]
                         , []
                         ]
                     }

extraUnits = Set.fromList [(Elerium, Generator), (Elerium, Chip), (Dilithium, Generator), (Dilithium, Chip)]

repState s@State{..} = s & floors . each . sets Set.map . _1 %~ fromJust . (`elemIndex` els)
    where els = nub $ s ^.. floors . each . folded . _1

check floor = null [() | (_, Generator) <- units]
            || and [(element, Generator) `Set.member` floor | (element, Chip) <- units]
            where units = Set.toList floor

isDone State{..} = _currentFloor == 3 && all null (Vec.take 3 _floors)

minSteps initialState = head [steps | (s, steps) <- states, isDone s] where
    states = bfsOn repState nextStates initialState
    nextStates State{..} = do
        nextFloor <- [pred _currentFloor, succ _currentFloor]
        guard (inRange (0, 3) nextFloor)
        size <- [1, 2]
        (moving, staying) <- pickSubset size (_floors Vec.! _currentFloor)
        let newNextFloor = (_floors Vec.! nextFloor) <> moving
        guard (check staying && check newNextFloor)
        pure $ State nextFloor (_floors Vec.// [(_currentFloor, staying), (nextFloor, newNextFloor)])

main = do
    print $ minSteps initialState
    print $ minSteps (initialState & floors . ix 0 <>~ extraUnits)
