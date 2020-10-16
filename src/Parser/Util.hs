module Parser.Util where


import Data.ArchCERES.Type
import Parser.ArchCERES.Type

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char


parseAWrapped :: Parser a -> Char -> Char -> Parser a
parseAWrapped parseKernel aOpeningChar aClosingChar = do
  void (char aOpeningChar)
  k <- parseKernel
  void (char aClosingChar)
  return k

parseAngleWrapped :: Parser a -> Parser a
parseAngleWrapped parseKernel = parseAWrapped parseKernel '<' '>'
parseRoundWrapped :: Parser a -> Parser a
parseRoundWrapped parseKernel = parseAWrapped parseKernel '(' ')'
parseCurlyWrapped :: Parser a -> Parser a
parseCurlyWrapped parseKernel = parseAWrapped parseKernel '{' '}'
parseSquareWrapped :: Parser a -> Parser a
parseSquareWrapped parseKernel = parseAWrapped parseKernel '[' ']'

parseListWithBy :: Parser a -> ScriptSource -> ScriptSource -> Parser [a]
parseListWithBy parseKernel delimiter closing = do
  undefined

parseListWithBy' :: Parser a -> Char -> Char -> Parser [a]
parseListWithBy' parseKernel delimiter closing = do
  undefined

parseSMapWithBy :: Parser a -> ScriptSource -> ScriptSource -> ScriptSource -> Parser (SMap a)
parseSMapWithBy parseKernel delimiter inter closing = do
  undefined

parseSMapWithBy' :: Parser a -> Char -> Char -> Char -> Parser (SMap a)
parseSMapWithBy' parseKernel delimiter inter closing = do
  undefined

