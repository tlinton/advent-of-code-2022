data Shape = 
    Rock |
    Paper |
    Scissor
    deriving (Eq, Show)

parseShape :: String -> Maybe Shape
parseShape [] = Nothing
parseShape (x:xs) = case x of
    'A' -> Just Rock
    'X' -> Just Rock
    'B' -> Just Paper
    'Y' -> Just Paper
    'C' -> Just Scissor
    'Z' -> Just Scissor
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

parseShapes :: String -> [Maybe Shape]
parseShapes line =
    map parseShape $ words line

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
    let shapes = map parseShapes $ lines contents
    let scores = map calculateScore shapes
    print $ sum scores
