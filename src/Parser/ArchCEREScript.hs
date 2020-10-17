module Parser.ArchCEREScript where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ArchCEREScript.Script
import Parser.ArchCEREScript.Type
import Parser.Util


parseArchCEREScript :: (Parser vp, Parser (vi vc), Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCEREScript s vp vi vc vt co eis)
parseArchCEREScript = parseControlInstruction
  -- (try (parseControlInstruction parsers)) <|> (parseManipulationInstruction parsers)

parseControlInstruction :: (Parser vp, Parser (vi vc), Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCEREScript s vp vi vc vt co eis)
parseControlInstruction parsers = do
  void (char 'S')
  choice [ parseSHaveNext, parseSEnd ]
 where
  parseSHaveNext = do
    partACS <- choice [parseSSeq, parseSSeqs, parseSLoop, parseSCase, parseSPar]
    void (string ">\n")
    acsNext <- parseArchCEREScript parsers
    return $ partACS acsNext
  parseSSeq = do
    void (string "Seq<")
    acs <- parseArchCERES parsers
    return $ SSeq acs
  parseSSeqs = do
    void (string "Seqs<")
    acsList <- parseList' (parseArchCERES parsers)
    return $ SSeqs acsList
  parseSLoop = do
    void (string "Loop<")
    loopCondition <- parseArchCEREScript parsers
    void (char ',')
    loopScript <- parseArchCEREScript parsers
    return $ SLoop loopCondition loopScript
  parseSCase = do
    void (string "Case<")
    branchCondition <- parseArchCEREScript parsers
    void (string ",")
    cases <- parseDefaultSMap (parseArchCEREScript parsers)
    return $ SCase branchCondition cases
  parseSPar = do
    void (string "Par<")
    pars <- parseDefaultSMap (parseArchCEREScript parsers)
    return $ SPar pars
  parseSEnd = do
    string "End."
    return SEnd

parseManipulationInstruction :: (Parser vp, Parser (vi vc), Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCERES vp vi vc vt co eis)
parseManipulationInstruction = parseArchCERES

parseArchCERES :: (Parser vp, Parser (vi vc), Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCERES vp vi vc vt co eis)
parseArchCERES parsers = do
  void (string "ArchCERES")
  return CRSNoop
