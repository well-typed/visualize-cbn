{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CBN.Parser (
    parseTerm
  , parseModule
  , term
  , parseIO
  ) where

import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Foldable (asum)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Pos (newPos)
import Text.Parsec.String

import qualified Language.Haskell.TH as TH
import qualified Text.Parsec.Token   as P

import CBN.Language
import CBN.Heap

{-------------------------------------------------------------------------------
  Quasi-quotation
-------------------------------------------------------------------------------}

term :: QuasiQuoter
term = QuasiQuoter {
      quoteExp = \str -> do
        l <- location
        c <- TH.runIO $ parseIO "splice" (setPosition l *> parseTerm) str
        dataToExpQ (const Nothing) c
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

{-------------------------------------------------------------------------------
  Individual parsers
-------------------------------------------------------------------------------}

parseVar :: Parser Var
parseVar = Var <$> identifier <?> "variable"

parseCon :: Parser Con
parseCon = (lexeme $ mkCon <$> upper <*> many alphaNum) <?> "constructor"
  where
    mkCon x xs = Con (x:xs)

parsePat :: Parser Pat
parsePat = Pat <$> parseCon <*> many parseVar

parseMatch :: Parser Match
parseMatch = Match <$> parsePat <* reservedOp "->" <*> parseTerm

-- | Parse a pointer
--
-- The only pointers we expect in initial terms are ones that refer to the
-- initial heap (the prelude)
parsePtr :: Parser Ptr
parsePtr = mkPtr <$ char '@' <*> identifier
  where
    mkPtr name = Ptr Nothing (Just name)

parseConApp :: Parser ConApp
parseConApp = ConApp <$> parseCon <*> many parseTermNoApp

parsePrimApp :: Parser PrimApp
parsePrimApp = PrimApp <$> parsePrim <*> many parseTermNoApp

parseTerm :: Parser Term
parseTerm = msum [
      TCon  <$> parseConApp
    , TPrim <$> parsePrimApp
    , TSeq  <$  reservedOp "seq" <*> parseTermNoApp <*> parseTermNoApp
    , nTApp <$> many1 parseTermNoApp
    ] <?> "term"

-- | Parser for terms without allowing for top-level application
parseTermNoApp :: Parser Term
parseTermNoApp =  msum [
      unaryTPrim <$> parsePrim
    , unaryTCon  <$> parseCon
    , TPtr  <$> parsePtr
    , TLam  <$  reservedOp "\\"
            <*> parseVar
            <*  reservedOp "->"
            <*> parseTerm
    , TLet  <$  reserved "let"
            <*> parseLetBound
            <*  reservedOp "in"
            <*> parseTerm
    , TIf   <$  reserved "if"
            <*> parseTerm
            <*  reserved "then"
            <*> parseTerm
            <*  reserved "else"
            <*> parseTerm
    , case1 <$  reserved "case"
            <*> parseTerm
            <*  reserved "of"
            <*> braces (parseMatch `sepBy` reservedOp ";")
    , case2 <$> parseSelector
            <*> parseTerm
    , TVar  <$> parseVar
    , parens parseTerm
    ]
  where
    unaryTPrim :: Prim -> Term
    unaryTPrim p = TPrim (PrimApp p [])

    unaryTCon :: Con -> Term
    unaryTCon c = TCon (ConApp c [])

    case1 :: Term -> [Match] -> Term
    case1 t ms = TCase t (Matches ms)

    case2 :: Selector -> Term -> Term
    case2 s t = TCase t (Selector s)

parseSelector :: Parser Selector
parseSelector = msum [
      Fst <$ reserved "fst"
    , Snd <$ reserved "snd"
    ]

parseLetBound :: Parser [(Var, Term)]
parseLetBound = asum [
      (:[]) <$> parseOne
    , braces (parseOne `sepBy` reservedOp ";")
    ]
  where
    parseOne :: Parser (Var, Term)
    parseOne =
        (,) <$> parseVar
            <*  reservedOp "="
            <*> parseTerm

parsePrim :: Parser Prim
parsePrim = msum [
      PInt   <$> natural
    , PISucc <$  reserved "succ"
    , PIAdd  <$  reserved "add"
    , PISub  <$  reserved "sub"
    , PIMul  <$  reserved "mul"
    , PIMin  <$  reserved "min"
    , PIMax  <$  reserved "max"
    , PILt   <$  reserved "lt"
    , PIEq   <$  reserved "eq"
    , PILe   <$  reserved "le"
    ]

-- | Our input files consist of an initial heap and the term to be evaluated
parseModule :: Parser (Heap Term, Term)
parseModule = (,) <$> (mkHeap <$> many parseFunDef) <*> parseMain
  where
    parseFunDef :: Parser (Var, Term)
    parseFunDef = (,) <$> parseVar
                      <*  reservedOp "="
                      <*> parseTermNoApp
                      <?> "function definition"

    parseMain :: Parser Term
    parseMain = reserved "main" >> reservedOp "=" *> parseTerm

    mkHeap :: [(Var, Term)] -> Heap Term
    mkHeap = initHeap . map (first varName)

{-------------------------------------------------------------------------------
  Lexical analysis
-------------------------------------------------------------------------------}

lexer = P.makeTokenParser haskellDef {
      P.reservedNames = [
          "case"
        , "of"
        , "let"
        , "in"
        , "succ"
        , "add"
        , "sub"
        , "mul"
        , "max"
        , "lt"
        , "eq"
        , "le"
        , "if"
        , "then"
        , "else"
        , "main"
        , "seq"
        , "fst"
        , "snd"
        ]
    , P.reservedOpNames = [
          "\\"
        , "->"
        , ";"
        , "@"
        , "="
        ]
    }

braces     = P.braces     lexer
identifier = P.identifier lexer
lexeme     = P.lexeme     lexer
natural    = P.natural    lexer
parens     = P.parens     lexer
reservedOp = P.reservedOp lexer
reserved   = P.reserved   lexer
whiteSpace = P.whiteSpace lexer

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

parseTopLevel :: Parser a -> Parser a
parseTopLevel p = whiteSpace *> p <* eof

parseIO :: String -> Parser a -> String -> IO a
parseIO input p str =
  case parse (parseTopLevel p) input str of
    Left err -> throwIO (userError (show err))
    Right a  -> return a

location :: Q SourcePos
location = aux <$> TH.location
  where
    aux :: TH.Loc -> SourcePos
    aux loc = uncurry (newPos (TH.loc_filename loc)) (TH.loc_start loc)
