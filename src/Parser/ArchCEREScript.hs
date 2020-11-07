module Parser.ArchCEREScript where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ArchCEREScript
import Parser.ArchCEREScript.Type
import Parser.Util


parseArchCEREScript :: Ord (v eis vp co) => (Parser eis, Parser (vP eis vi vc v vp vt co), Parser (vi eis vc v vp vt co), Parser (vc eis v vp co), Parser (v eis vp co), Parser vp, Parser vt, Parser co) -> Parser (ArchCEREScript eis vP vi vc v vp vt co)
parseArchCEREScript parsers@(_, _, _, _, parseValueWith, _, _, _) = do
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
    parseLabel = error "[ERROR]<parseArchCEREScript:=:parseLabel> Not yet implemented"
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


-- TODO: Not yet implemented
parseArchCERES :: (Parser eis, Parser (vP eis vi vc v vp vt co), Parser (vi eis vc v vp vt co), Parser (vc eis v vp co), Parser (v eis vp co), Parser vp, Parser vt, Parser co) -> Parser (ArchCERES eis vP vi vc v vp vt co)
parseArchCERES parsers@(parseEISWith, parseVPWith, parseVIWith, parseVCWith, parseVWith, parseVP, parseVT, parseCO) =
  choice
    [ parseNoop
    , parseRun
    , parseReturn
    , parseError
    , parseBreak
    , parseClearVariables
    , parseInitVariable
    , parseInitVariableAt
    , parseInitVariableBy
    , parseCheckVariable
    , parseDeleteVariable
    , parseDo
    , parseModifyValue
    , parseModifyValue1
    , parseModifyValue2
    , parseModifyValue3
    , parseModifyValue4
    , parseModifyBothValue
    , parseModifyBothValue1
    , parseModifyBothValue2
    , parseModifyBothValue3
    , parseModifyBothValue4
    , parseModifyValues
    , parseCopyValue
    , parseConvertValue
    , parseRandom
    , parseLog
      parseExt
    ]
 where
  parseNoop = do
    string "Noop"
    return CRSNoop
  parseRun = error "[ERROR]<parseArchCERES:=:parseRun> Not yet implemented"
  parseReturn = error "[ERROR]<parseArchCERES:=:parseReturn> Not yet implemented"
  parseError = error "[ERROR]<parseArchCERES:=:parseError> Not yet implemented"
  parseBreak = error "[ERROR]<parseArchCERES:=:parseBreak> Not yet implemented"
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
  parseInitVariableBy = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseCheckVariable = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseDeleteVariable = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseDo = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyValue = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyValue1 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyValue2 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyValue3 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyValue4 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyBothValue = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyBothValue1 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyBothValue2 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyBothValue3 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyBothValue4 = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseModifyValues = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseCopyValue = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseConvertValue = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseRandom = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseLog = error "[ERROR]<parseArchCERES:=:parse> Not yet implemented"
  parseExt = do
    eis <- parseEISWith
    return $ CRSExt eis
  consumeASpace :: Parser ()
  consumeASpace = void (char ' ')
