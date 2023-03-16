{-# LANGUAGE NamedFieldPuns #-}
import qualified Data.Stack as St
import Data.Vector ((!?))
import qualified Data.Vector as Vec
import Text.Parsec ((<|>))
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
        crateLine = Parsec.choice [crate, empty] `Parsec.sepBy1` Parsec.char ' '
        col = do
            idx <- Parsec.many1 Parsec.digit
            return $ Col (read idx)
        colLine = Parsec.char ' ' >> col `Parsec.sepEndBy` Parsec.many1 (Parsec.char ' ')
        moveLine = do
            Parsec.string "move "
            count <- Parsec.many1 Parsec.digit
            Parsec.string " from "
            src <- Parsec.many1 Parsec.digit
            Parsec.string " to "
            dst <- Parsec.many1 Parsec.digit
            return [Move (read count) (read src) (read dst)]
        line = Parsec.choice [Parsec.try crateLine, Parsec.try colLine, Parsec.try moveLine]
        parser = line `Parsec.sepEndBy` Parsec.many1 Parsec.endOfLine

pushData :: Stacks -> [Maybe Char] -> Stacks
pushData stacks newData =
    Vec.fromList $ zipWith updateStack [0..] newData
    where
        updateStack id value = case stacks !? id of
            Just s -> pushValue s value
            Nothing -> pushValue St.stackNew value
        pushValue stack (Just value) = St.stackPush stack value
        pushValue stack Nothing = stack

runLine :: Stacks -> [Token] -> Stacks
runLine stacks (Empty:rest) = stacks
runLine stacks (Move{count, src, dst}:rest) = stacks

runInstructions :: Stacks -> [[Token]] -> Stacks
runInstructions stacks [] = stacks
runInstructions stacks (instructions:rest) = runInstructions (runLine stacks instructions) rest

main :: IO ()
main = do
    instructions <- getContents
    let stacks = createStacks
    --print $ pushData (pushData stacks [Just 'A', Nothing, Just 'C']) [Just 'B', Just 'z', Just 'F']
    --print $ parseLine "[A] [B]"
    --print $ map parseLine (lines instructions)
    print $ parseInstructions instructions
