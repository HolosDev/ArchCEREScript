module Parser.ArchCEREScript where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ArchCEREScript.Script
import Parser.ArchCEREScript.Type
import Parser.ArchCEREScript.VariablePosition
import Parser.Util


parseArchCEREScript :: Ord vc => (Parser vp, Parser vi, Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCEREScript s vp vi vc vt co eis)
parseArchCEREScript = parseControlInstruction
-- (try (parseControlInstruction parsers)) <|> (parseManipulationInstruction parsers)

parseControlInstruction :: Ord vc => (Parser vp, Parser vi, Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCEREScript s vp vi vc vt co eis)
parseControlInstruction parsers@(_, _, parseVC, _, _, _) = do
  void (char 'S')
  choice [parseSHaveNext, parseSEnd]
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
    cases <- parseDefaultMap parseVC (parseArchCEREScript parsers)
    void (string ",")
    otherwiseScript <- parseArchCEREScript parsers
    return $ SCase branchCondition cases otherwiseScript
  parseSPar = do
    void (string "Par<")
    pars <- parseList' (parseArchCEREScript parsers)
    return $ SPar pars
  parseSEnd = do
    string "End."
    return SEnd

parseManipulationInstruction :: (Parser vp, Parser vi, Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCERES vp vi vc vt co eis)
parseManipulationInstruction = parseArchCERES

-- TODO: Not yet implemented
parseArchCERES :: (Parser vp, Parser vi, Parser vc, Parser vt, Parser co, Parser eis) -> Parser (ArchCERES vp vi vc vt co eis)
parseArchCERES parsers@(parseVP, parseVIWith, parseVC, parseVT, parseCO, parseEIS)
  = choice
    [ parseNoop
    , parseClearVariable
    , parseInitVariable
    , parseInitVariableAt
    {-
    , parseCheckVariable
    , parseDeleteVariable
    , parseModifyValue1
    , parseModifyValue2
    , parseModifyValue3
    , parseModifyValue4
    , parseCopyValue
    , parseConvertValue
    , parseConvertValueBy
    , parseConvertValueWith
    , parseReplaceText
    , parseReplaceTextTo
    , parseReplaceTextBy
    , parseReplaceTextByTo
    , parseGetPointer
    , parseSetPointer
    , parseRandom
    , parseRandomBy
    , parseRandomWith
    , parseRandomWithBy
    , parseParseScript
    , parseLog
    , parseTo0
    , parseTo1
    , parseTo2
    , parseTo3
    , parseTo4
    , parseTo5
    , parseTo6
    , parseTo7
    , parseTo8
    , parseToList
    -}
    , parseExt
    ]
 where
  parseNoop = do
    string "Noop"
    return CRSNoop
  parseClearVariable = do
    string "ClearVariable"
    consumeASpace
    vp <- parseVP
    return $ CRSClearVariable vp
  parseInitVariable = do
    string "InitVariable"
    consumeASpace
    vP1 <- (parseVariablePosition undefined undefined undefined)
    consumeASpace
    vP2 <- (parseVariablePosition undefined undefined undefined)
    return $ CRSInitVariable vP1 vP2
  parseInitVariableAt = do
    string ""
    vp <- parseVP
    consumeASpace
    vP <- (parseVariablePosition undefined undefined undefined)
    return $ CRSInitVariableAt vp vP
  {-
  parseCheckVariable = do
    string ""
    return CRS
  parseDeleteVariable = do
    string ""
    return CRS
  parseModifyValue1 = do
    string ""
    return CRS
  parseModifyValue2 = do
    string ""
    return CRS
  parseModifyValue3 = do
    string ""
    return CRS
  parseModifyValue4 = do
    string ""
    return CRS
  parseCopyValue = do
    string ""
    return CRS
  parseConvertValue = do
    string ""
    return CRS
  parseConvertValueBy = do
    string ""
    return CRS
  parseConvertValueWith = do
    string ""
    return CRS
  parseReplaceText = do
    string ""
    return CRS
  parseReplaceTextTo = do
    string ""
    return CRS
  parseReplaceTextBy = do
    string ""
    return CRS
  parseReplaceTextByTo = do
    string ""
    return CRS
  parseGetPointer = do
    string ""
    return CRS
  parseSetPointer = do
    string ""
    return CRS
  parseRandom = do
    string ""
    return CRS
  parseRandomBy = do
    string ""
    return CRS
  parseRandomWith = do
    string ""
    return CRS
  parseRandomWithBy = do
    string ""
    return CRS
  parseParseScript = do
    string ""
    return CRS
  parseLog = do
    string ""
    return CRS
  parseTo0 = do
    string ""
    return CRS
  parseTo1 = do
    string ""
    return CRS
  parseTo2 = do
    string ""
    return CRS
  parseTo3 = do
    string ""
    return CRS
  parseTo4 = do
    string ""
    return CRS
  parseTo5 = do
    string ""
    return CRS
  parseTo6 = do
    string ""
    return CRS
  parseTo7 = do
    string ""
    return CRS
  parseTo8 = do
    string ""
    return CRS
  parseToList = do
    string ""
    return CRS
    -}
  parseExt = return CRSNoop -- Use parseEIS
  consumeASpace :: Parser ()
  consumeASpace = void (char ' ')
