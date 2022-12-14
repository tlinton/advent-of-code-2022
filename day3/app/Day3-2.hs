import Data.Char
import Data.Ix
import Data.List

--findBadge :: Eq a => [a] -> [a]
findBadge :: [String] -> [String]
findBadge (a:b:c:rest) =
    badge : findBadge rest
    where
        badge = take 1 $ a `intersect` b `intersect` c
findBadge (a:rest) = []
findBadge [] = []

getPriority :: String -> Int
getPriority (item:rest)
    | inRange ('a', 'z') item = ord item - ord 'a' + 1
    | inRange ('A', 'Z') item = ord item - ord 'A' + 27
    | otherwise = 0
getPriority [] = 0

main :: IO ()
main = do
    contents <- getContents
    print $ sum $ map getPriority $ findBadge $ lines contents
    --print $ sum $ map (getPriority . getCommonItem) $ lines contents
