module Parser.Util where


import Control.Monad (void)
import Data.IntMap.Strict as SIM
import Data.Map.Strict as SM
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


import Data.ArchCEREScript.Type
import Parser.ArchCEREScript.Type


parseAWrapped :: Parser a -> ScriptSource -> ScriptSource -> Parser a
parseAWrapped parseKernel opening closing = do
  void (string opening)
  k <- parseKernel
  void (string closing)
  return k

parseAWrapped' :: Parser a -> Char -> Char -> Parser a
parseAWrapped' parseKernel aOpeningChar aClosingChar = do
  void (char aOpeningChar)
  k <- parseKernel
  void (char aClosingChar)
  return k

parseAngleWrapped :: Parser a -> Parser a
parseAngleWrapped parseKernel = parseAWrapped' parseKernel '<' '>'
parseRoundWrapped :: Parser a -> Parser a
parseRoundWrapped parseKernel = parseAWrapped' parseKernel '(' ')'
parseCurlyWrapped :: Parser a -> Parser a
parseCurlyWrapped parseKernel = parseAWrapped' parseKernel '{' '}'
parseSquareWrapped :: Parser a -> Parser a
parseSquareWrapped parseKernel = parseAWrapped' parseKernel '[' ']'

parseListWithBy :: Parser a -> ScriptSource -> ScriptSource -> ScriptSource -> Parser [a]
parseListWithBy parseValue opening delimiter closing = do
  void (string opening)
  vList <- parseListKernel parseValue (void (string delimiter))
  void (string closing)
  return vList

parseListWithBy' :: Parser a -> Char -> Char -> Char -> Parser [a]
parseListWithBy' parseValue opening delimiter closing = do
  void (char opening)
  vList <- parseListKernel parseValue (void (char delimiter))
  void (char closing)
  return vList

parseListWith :: Parser a -> ScriptSource -> Parser [a]
parseListWith parseValue delimiter = do
  vList <- parseSquareWrapped (parseListKernel parseValue (void (string delimiter)))
  return vList

parseListWith' :: Parser a -> Char -> Parser [a]
parseListWith' parseValue delimiter = do
  vList <- parseSquareWrapped (parseListKernel parseValue (void (char delimiter)))
  return vList

parseList :: Parser a -> Parser [a]
parseList parseValue = do
  vList <- parseSquareWrapped (parseListKernel parseValue (void (string "::")))
  return vList

parseList' :: Parser a -> Parser [a]
parseList' parseValue = do
  vList <- parseSquareWrapped (parseListKernel parseValue (void (char ',')))
  return vList

parseListKernel :: Parser a -> Parser d -> Parser [a]
parseListKernel parseValue parseDelimiter = do
  mV <- optional parseValue
  maybe (pure []) (\v -> fmap (v :) (many parseValueWithDelimiter)) mV
 where
  parseValueWithDelimiter = do
    void parseDelimiter
    parseValue

parseIntMapWithBy :: Parser a -> ScriptSource -> ScriptSource -> ScriptSource -> ScriptSource -> Parser (IntMap a)
parseIntMapWithBy parseValue opening delimiter inter closing = do
  void (string opening)
  kvList <- parseListKernel (parseKVBy parseSignedInt parseValue inter) (string delimiter)
  void (string closing)
  return . SIM.fromList $ kvList

parseIntMapWithBy' :: Parser a -> Char -> Char -> Char -> Char -> Parser (IntMap a)
parseIntMapWithBy' parseValue opening delimiter inter closing = do
  void (char opening)
  kvList <- parseListKernel (parseKVBy' parseSignedInt parseValue inter) (char delimiter)
  void (char closing)
  return . SIM.fromList $ kvList

parseIntMapWith :: Parser a -> ScriptSource -> ScriptSource -> Parser (IntMap a)
parseIntMapWith parseValue delimiter inter = do
  kvList <- parseSquareWrapped (parseListKernel (parseKVBy parseSignedInt parseValue inter) (string delimiter))
  return . SIM.fromList $ kvList

parseIntMapWith' :: Parser a -> Char -> Char -> Parser (IntMap a)
parseIntMapWith' parseValue delimiter inter = do
  kvList <- parseSquareWrapped (parseListKernel (parseKVBy' parseSignedInt parseValue inter) (char delimiter))
  return . SIM.fromList $ kvList

parseDefaultIntMap :: Parser a -> Parser (IntMap a)
parseDefaultIntMap parseValue = do
  kvList <- parseSquareWrapped (parseListKernel (parseKV parseSignedInt parseValue) (char ','))
  return . SIM.fromList $ kvList

parseMapWithBy :: Ord idx => Parser idx -> Parser a -> ScriptSource -> ScriptSource -> ScriptSource -> ScriptSource -> Parser (Map idx a)
parseMapWithBy parseIdx parseValue opening delimiter inter closing = do
  void (string opening)
  kvList <- parseListKernel (parseKVBy parseIdx parseValue inter) (string delimiter)
  void (string closing)
  return . SM.fromList $ kvList

parseMapWithBy' :: Ord idx => Parser idx -> Parser a -> Char -> Char -> Char -> Char -> Parser (Map idx a)
parseMapWithBy' parseIdx parseValue opening delimiter inter closing = do
  void (char opening)
  kvList <- parseListKernel (parseKVBy' parseIdx parseValue inter) (char delimiter)
  void (char closing)
  return . SM.fromList $ kvList

parseMapWith :: Ord idx => Parser idx -> Parser a -> ScriptSource -> ScriptSource -> Parser (Map idx a)
parseMapWith parseIdx parseValue delimiter inter = do
  kvList <- parseSquareWrapped (parseListKernel (parseKVBy parseIdx parseValue inter) (string delimiter))
  return . SM.fromList $ kvList

parseMapWith' :: Ord idx => Parser idx -> Parser a -> Char -> Char -> Parser (Map idx a)
parseMapWith' parseIdx parseValue delimiter inter = do
  kvList <- parseSquareWrapped (parseListKernel (parseKVBy' parseIdx parseValue inter) (char delimiter))
  return . SM.fromList $ kvList

parseDefaultMap :: Ord idx => Parser idx -> Parser a -> Parser (Map idx a)
parseDefaultMap parseIdx parseValue = do
  kvList <- parseSquareWrapped (parseListKernel (parseKV parseIdx parseValue) (char ','))
  return . SM.fromList $ kvList

parseKVBy :: Parser idx -> Parser v -> ScriptSource -> Parser (idx, v)
parseKVBy parseIdx parseValue inter = parseRoundWrapped (parseKVKernelBy parseIdx parseValue inter)

parseKVBy' :: Parser idx -> Parser v -> Char -> Parser (idx, v)
parseKVBy' parseIdx parseValue inter = parseRoundWrapped (parseKVKernelBy' parseIdx parseValue inter)

parseKV :: Parser idx -> Parser v -> Parser (idx, v)
parseKV parseIdx parseValue = parseRoundWrapped (parseKVKernel parseIdx parseValue)

parseKVKernelBy :: Parser idx -> Parser v -> ScriptSource -> Parser (idx, v)
parseKVKernelBy parseIdx parseValue inter = do
  idx <- parseIdx
  void (string inter)
  v <- parseValue
  return (idx, v)

parseKVKernelBy' :: Parser idx -> Parser v -> Char -> Parser (idx, v)
parseKVKernelBy' parseIdx parseValue inter = do
  idx <- parseIdx
  void (char inter)
  v <- parseValue
  return (idx, v)

parseKVKernel :: Parser idx -> Parser v -> Parser (idx, v)
parseKVKernel parseIdx parseValue = do
  idx <- parseIdx
  void (char '|')
  v <- parseValue
  return (idx, v)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parseInt :: Parser Int
parseInt = lexeme L.decimal

parseSignedInt :: Parser Int
parseSignedInt = L.signed space parseInt
