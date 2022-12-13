data Shape = 
    Rock |
    Paper |
    Scissor
    deriving (Eq, Show)

parseShape :: String -> Maybe Shape
parseShape [] = Nothing
parseShape (x:xs) = case x of
    'A' -> Just Rock
    'B' -> Just Paper
    'C' -> Just Scissor
    _ -> Nothing

data Outcome =
    ILose |
    Draw |
    IWin
    deriving (Eq, Show)

parseOutcome :: String -> Maybe Outcome
parseOutcome [] = Nothing
parseOutcome (x:xs) = case x of
    'X' -> Just ILose
    'Y' -> Just Draw
    'Z' -> Just IWin
    _ -> Nothing

instance Ord Shape where 
    -- compare :: Shape -> Shape -> Ordering
    compare Rock Rock = EQ
    compare Rock Paper = LT
    compare Rock Scissor = GT
    compare Paper Rock = GT
    compare Paper Paper = EQ
    compare Paper Scissor = LT
    compare Scissor Rock = LT
    compare Scissor Paper = GT
    compare Scissor Scissor = EQ

parseInstructions :: String -> Maybe (Shape, Outcome)
parseInstructions line =
    parseParts $ words line
    where
        parseParts (a : b : _) = combineTuple (parseShape a) (parseOutcome b)
        parseParts _ = Nothing
        combineTuple (Just a) (Just b) = Just (a, b)
        combineTuple _ _ = Nothing

createShapes :: Maybe (Shape, Outcome) -> [Maybe Shape]
createShapes (Just (Rock, ILose)) = [Just Rock, Just Scissor]
createShapes (Just (Rock, IWin)) = [Just Rock, Just Paper]
createShapes (Just (Paper, ILose)) = [Just Paper, Just Rock]
createShapes (Just (Paper, IWin)) = [Just Paper, Just Scissor]
createShapes (Just (Scissor, ILose)) = [Just Scissor, Just Paper]
createShapes (Just (Scissor, IWin)) = [Just Scissor, Just Rock]
createShapes (Just (a, Draw)) = [Just a, Just a]
createShapes Nothing = [Nothing, Nothing]

calculateScore :: [Maybe Shape] -> Int
calculateScore (Just h1 : Just h2 : _) =
    shapeScore h2 +  outcomeScore h1 h2
    where
        shapeScore Rock = 1
        shapeScore Paper = 2
        shapeScore Scissor = 3
        outcomeScore h1 h2
            | h1 > h2 = 0
            | h1 == h2 = 3
            | h1 < h2 = 6


main :: IO()
main = do
    contents <- getContents
    let shapes = map (createShapes . parseInstructions) $ lines contents
    let scores = map calculateScore shapes
    print $ sum scores
