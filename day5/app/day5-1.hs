import qualified Data.Stack as St
import Data.Vector ((!?))
import qualified Data.Vector as Vec
import qualified Text.Parsec as Parsec
import qualified Data.Char as Parsec

type Stacks = Vec.Vector (St.Stack Char)

data Token = Empty
    | Crate { id :: Char }
    | Move { count :: Int, src :: Int, dst :: Int }
    | Col { idx :: Int }
    deriving (Eq, Show)

createStacks :: Stacks
createStacks = Vec.empty

parseInstructions :: String -> Either Parsec.ParseError [[Token]]
parseInstructions = Parsec.parse parser ""
    where
        empty = Parsec.count 3 Parsec.space >> return Empty
        crate = do
            Parsec.char '['
            crateId <- Parsec.anyChar
            Parsec.char ']'
            return $ Crate crateId
        col = do
            idx <- Parsec.many Parsec.digit
            return $ Col (read idx)
        move = do
            Parsec.string "move "
            count <- Parsec.many Parsec.digit
            Parsec.string " from "
            src <- Parsec.many Parsec.digit
            Parsec.string " to "
            dst <- Parsec.many Parsec.digit
            return $ Move (read count) (read src) (read dst)
        line = Parsec.choice [empty, crate, col, move] `Parsec.sepBy` Parsec.char ' '
        parser = line `Parsec.endBy` Parsec.endOfLine

pushData :: Stacks -> [Maybe Char] -> Stacks
pushData stacks newData =
    Vec.fromList $ zipWith updateStack [0..] newData
    where
        updateStack id value = case stacks !? id of
            Just s -> pushValue s value
            Nothing -> pushValue St.stackNew value
        pushValue stack (Just value) = St.stackPush stack value
        pushValue stack Nothing = stack

main :: IO ()
main = do
    instructions <- getContents
    let stacks = createStacks
    --print $ pushData (pushData stacks [Just 'A', Nothing, Just 'C']) [Just 'B', Just 'z', Just 'F']
    --print $ parseLine "[A] [B]"
    --print $ map parseLine (lines instructions)
    print $ parseInstructions instructions
