import Data.Maybe (mapMaybe)
import Text.Parsec as Parsec

data Sections = Sections {
    first :: Int,
    second :: Int
} deriving (Show)

contains :: Sections -> Sections -> Bool
a `contains` b = first a <= first b && second a >= second b

parseSections :: String -> Maybe (Sections, Sections)
parseSections line =
    case Parsec.parse parser "" line of
        Left _ -> Nothing
        Right result -> Just result
    where
        parser = do
            a <- Parsec.many Parsec.digit
            Parsec.char '-'
            b <- Parsec.many Parsec.digit
            Parsec.char ','
            c <- Parsec.many Parsec.digit
            Parsec.char '-'
            d <- Parsec.many Parsec.digit
            return (Sections (read a) (read b), Sections (read c) (read d))

areOverlapping :: Sections -> Sections -> Bool
areOverlapping a b = a `contains` b || b `contains` a

main :: IO ()
main = do
    contents <- getContents
    let sections = mapMaybe parseSections $ lines contents
    print $ length $ filter id $ map (uncurry areOverlapping) sections
