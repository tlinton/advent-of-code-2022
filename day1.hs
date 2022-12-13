import Data.List
import Text.Read

data Elf = Elf {
    elfId :: Int,
    calories :: Int
} deriving (Show)

newElf :: Elf -> Int -> Elf
newElf elf newCalories =
    Elf { elfId = elfId elf + 1, calories = newCalories }

updateCalories :: Elf -> Int -> Elf
updateCalories elf moreCalories =
    Elf { elfId = elfId elf, calories = calories elf + moreCalories}

data State = State {
    hasElf :: Bool,
    elves :: [Elf]
} deriving (Show)

initState = State {
    hasElf = False,
    elves = []
}

handleLine :: State -> String -> State
handleLine inputState line =
    case readMaybe line of
        Just calories -> handleCalories inputState calories
        Nothing -> inputState { hasElf = False }
    where
        handleCalories state calories =
            if hasElf state then
                state { elves = updateElf (elves state) calories }
            else
                state { hasElf = True, elves = addElf (elves state) calories }
            where
                updateElf [] calories =
                    [Elf { elfId = 1, calories = calories }]
                updateElf (elf:elves) calories =
                    updateCalories elf calories : elves
                addElf [] calories =
                    [Elf { elfId = 1, calories = calories }]
                addElf (elf:elves) calories =
                    newElf elf calories : elf : elves

reverseCompare :: Ord a => a -> a -> Ordering
reverseCompare a b
    | a < b = GT
    | a == b = EQ
    | a > b = LT

main = do
    contents <- getContents
    let elfData = foldl handleLine initState (lines contents)
    print $ sum $ take 3 $ sortBy reverseCompare $ map calories (elves elfData)
