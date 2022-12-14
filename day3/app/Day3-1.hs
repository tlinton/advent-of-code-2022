import Data.Char
import Data.Ix
import Data.List

getCommonItem :: Eq a => [a] -> [a]
getCommonItem line =
    take 1 $ c1 `intersect` c2
    where
        (c1, c2) = splitAt size line
        size = length line `div` 2

getPriority :: String -> Int
getPriority (item:rest)
    | inRange ('a', 'z') item = ord item - ord 'a' + 1
    | inRange ('A', 'Z') item = ord item - ord 'A' + 27
    | otherwise = 0
getPriority [] = 0

main :: IO ()
main = do
    contents <- getContents
    print $ sum $ map (getPriority . getCommonItem) $ lines contents
