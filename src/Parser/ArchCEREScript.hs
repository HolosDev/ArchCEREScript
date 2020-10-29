module Parser.ArchCEREScript where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ArchCEREScript
import Parser.ArchCEREScript.Type
import Parser.Util


parseArchCEREScript :: Ord (v eis vp co) => (Parser eis, Parser (vP eis vi vc v vp vt co), Parser (vi eis vc v vp vt co), Parser (vc eis v vp co), Parser (v eis vp co), Parser vp, Parser vt, Parser co) -> Parser (ArchCEREScript eis vP vi vc v vp vt co)
parseArchCEREScript = parseControlInstruction

-- (try (parseControlInstruction parsers)) <|> (parseManipulationInstruction parsers)

parseControlInstruction :: Ord (v eis vp co) => (Parser eis, Parser (vP eis vi vc v vp vt co), Parser (vi eis vc v vp vt co), Parser (vc eis v vp co), Parser (v eis vp co), Parser vp, Parser vt, Parser co) -> Parser (ArchCEREScript eis vP vi vc v vp vt co)
parseControlInstruction parsers@(_, _, _, _, parseValueWith, _, _, _) = do
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
    void (char ',')
    loopLabel <- parseLabel
    return $ SLoop loopCondition loopScript loopLabel
   where
    parseLabel = undefined
  parseSCase = do
    void (string "Case<")
    branchCondition <- parseArchCEREScript parsers
    void (string ",")
    cases <- parseDefaultMap parseValueWith (parseArchCEREScript parsers)
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

parseManipulationInstruction :: (Parser eis, Parser (vP eis vi vc v vp vt co), Parser (vi eis vc v vp vt co), Parser (vc eis v vp co), Parser (v eis vp co), Parser vp, Parser vt, Parser co) -> Parser (ArchCERES eis vP vi vc v vp vt co)
parseManipulationInstruction = parseArchCERES

-- TODO: Not yet implemented
parseArchCERES :: (Parser eis, Parser (vP eis vi vc v vp vt co), Parser (vi eis vc v vp vt co), Parser (vc eis v vp co), Parser (v eis vp co), Parser vp, Parser vt, Parser co) -> Parser (ArchCERES eis vP vi vc v vp vt co)
parseArchCERES parsers@(parseEISWith, parseVPWith, parseVIWith, parseVCWith, parseVWith, parseVP, parseVT, parseCO) =
  choice
    [ parseNoop
    , parseClearVariables
    , parseInitVariable
    , parseInitVariableAt
    , {-
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
      parseExt
    ]
 where
  parseNoop = do
    string "Noop"
    return CRSNoop
  parseClearVariables = do
    string "ClearVariables"
    consumeASpace
    vp <- parseVP
    return $ CRSClearVariables vp
  parseInitVariable = do
    string "InitVariable"
    consumeASpace
    wVP1 <- parseVPWith
    return $ CRSInitVariable wVP1
  parseInitVariableAt = do
    string ""
    vp <- parseVP
    consumeASpace
    wVP1 <- parseVPWith
    return $ CRSInitVariableAt vp wVP1
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
  parseExt = do
    eis <- parseEISWith
    return $ CRSExt eis
  consumeASpace :: Parser ()
  consumeASpace = void (char ' ')
