module Day11 where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Lens.Micro.Platform

import AOC

data UnitType = Chip | Generator
              deriving (Eq, Ord)

data Element = Strontium | Plutonium | Thulium | Ruthenium | Curium | Elerium | Dilithium
             deriving (Eq, Ord)

data Unit a = (:@:) { _element :: a
                    , _unitType :: UnitType
                    } deriving (Eq, Ord)

makeLenses ''Unit

data State a = State { _currentFloor :: Int
                     , _floors :: Vector (Set (Unit a))
                     } deriving (Eq, Ord)

makeLenses ''State

initialState :: State Element
initialState = State { _currentFloor = 0
                     , _floors = Vec.fromList $ map Set.fromList
                         [ [Strontium :@: Generator, Strontium :@: Chip, Plutonium :@: Generator, Plutonium :@: Chip]
                         , [Thulium :@: Generator, Ruthenium :@: Generator, Ruthenium :@: Chip, Curium :@: Generator, Curium :@: Chip]
                         , [Thulium :@: Chip]
                         , []
                         ]
                     }

extraUnits :: Set (Unit Element)
extraUnits = Set.fromList [Elerium :@: Generator, Elerium :@: Chip, Dilithium :@: Generator, Dilithium :@: Chip]

-- Two states are considered equivalent if they only differ by a permutation of elements
upToElementsPermutation :: Eq a => State a -> State Int
upToElementsPermutation s@State{..} = s & floors . each . sets Set.map . element %~ fromJust . (`elemIndex` elementsOrder)
    where elementsOrder = nub $ s ^.. floors . each . folded . element

check :: Ord a => Set (Unit a) -> Bool
check floor = null [() | _ :@: Generator <- units]
            || and [element :@: Generator `Set.member` floor | element :@: Chip <- units]
            where units = Set.toList floor

nextStates :: Ord a => State a -> [State a]
nextStates State{..} = do
    nextFloor <- [pred _currentFloor, succ _currentFloor]
    guard (inRange (0, 3) nextFloor)
    size <- [1, 2]
    (moving, newCurrentFloor) <- pickSubset size (_floors Vec.! _currentFloor)
    let newNextFloor = (_floors Vec.! nextFloor) <> moving
    guard (check newCurrentFloor && check newNextFloor)
    pure $ State nextFloor (_floors Vec.// [(_currentFloor, newCurrentFloor), (nextFloor, newNextFloor)])

isDone :: State a -> Bool
isDone State{..} = _currentFloor == 3 && all null (Vec.take 3 _floors)

minSteps :: Ord a => State a -> Integer
minSteps start = head [steps | (s, steps) <- states, isDone s]
    where states = bfsOn upToElementsPermutation nextStates start

main = do
    print $ minSteps initialState
    print $ minSteps (initialState & floors . ix 0 <>~ extraUnits)
