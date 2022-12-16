import Data.Ix (inRange)
import Data.Maybe (mapMaybe)
import Text.Parsec as Parsec

data Sections = Sections {
    first :: Int,
    second :: Int
} deriving (Show)

overlapping :: Sections -> Sections -> Bool
a `overlapping` b = partial_overlap b_sections a || partial_overlap a_sections b
    where
        partial_overlap range sections = inRange range (first sections) || inRange range (second sections)
        a_sections = (first a, second a)
        b_sections = (first b, second b)


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

main :: IO ()
main = do
    contents <- getContents
    let sections = mapMaybe parseSections $ lines contents
    print $ length $ filter id $ map (uncurry overlapping) sections
