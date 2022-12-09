import Text.Read

data State = State {
    hasElf :: Bool,
    currentElf :: Int,
    currentCalories :: Int,
    maxElf :: Int,
    maxCalories :: Int
} deriving (Show)

initState = State {
    hasElf = False,
    currentElf = 0,
    currentCalories = 0,
    maxElf = 0,
    maxCalories = 0
}

handleLine :: State -> String -> State
handleLine inputState line =
    case readMaybe line of
        Just calories -> handleCalories inputState calories
        Nothing -> inputState { hasElf = False }
    where
        handleCalories state calories =
            if hasElf state then
                updateMax state { currentCalories = currentCalories state + calories }
            else
                updateMax state { hasElf = True, currentElf = currentElf state + 1, currentCalories = calories }
        updateMax state =
            if currentCalories state > maxCalories state then
                state { maxCalories = currentCalories state, maxElf = currentElf state}
            else
                state

main = do
    content <- getContents
    print $ foldl handleLine initState (lines content)
