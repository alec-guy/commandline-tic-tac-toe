module Parser where 

import Text.Megaparsec 
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer
import Data.Set as Set -- for parsing failures 
import Control.Monad.Combinators 
import Data.Int (Int8 (..)) -- small is what we need
import Control.Monad (void)
import Data.Void (Void)
import Data.List as List 

type Parser = Parsec Void String 

spaceParser :: Parser () 
spaceParser = Text.Megaparsec.Char.Lexer.space hspace1 Control.Monad.Combinators.empty Control.Monad.Combinators.empty  

lexemeParser :: Parser a -> Parser a 
lexemeParser = Text.Megaparsec.Char.Lexer.lexeme spaceParser 

newtype Row    =    Row Int8 deriving (Eq, Show)
newtype Column =    Column Int8 deriving (Eq, Show)
data Control = Control Row Column deriving (Eq, Show)

parseRow :: Parser Row 
parseRow = do 
    void $ lexemeParser (string "r:")
    num <- lexemeParser $ List.singleton <$> digitChar
    case num <= "3" && num > "0" of 
        True  -> return $ Row $ (read num :: Int8)
        False -> fancyFailure $ Set.singleton $ ErrorFail "row out of range"

parseColumn :: Parser Column  
parseColumn = do 
    void $ lexemeParser (string "c:")
    num <- lexemeParser $ List.singleton <$> digitChar
    case num <= "3" && num > "0" of 
        True  -> return $ Column $ (read num :: Int8)
        False -> fancyFailure $ Set.singleton $ ErrorFail "column out of range"

parseControl :: Parser Control 
parseControl = do 
    row    <- parseRow 
    column <- parseColumn 
    eof 
    return $ Control row column

parseMove :: String -> Either (ParseErrorBundle String Void) Control 
parseMove move = parse parseControl "" move 