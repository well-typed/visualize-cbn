{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CBN.Parser (
    parseTerm
  , parseTopLevel
  , term
  ) where

import Control.Exception
import Control.Monad
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Pos (newPos)
import Text.Parsec.String
import qualified Text.Parsec.Token   as P
import qualified Language.Haskell.TH as TH

import CBN.Language
import CBN.Heap

{-------------------------------------------------------------------------------
  Quasi-quotation
-------------------------------------------------------------------------------}

term :: QuasiQuoter
term = QuasiQuoter {
      quoteExp = \str -> do
        l <- location
        c <- TH.runIO $ parseIO (setPosition l *> parseTopLevel parseTerm) str
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

parsePtr :: Parser Ptr
parsePtr = (mkPtr <$ reservedOp "@" <*> integer) <?> "pointer"
  where
    mkPtr = Ptr . fromInteger

parsePat :: Parser Pat
parsePat = Pat <$> parseCon <*> many parseVar

parseMatch :: Parser Match
parseMatch = Match <$> parsePat <* reservedOp "->" <*> parseTerm

parseTerm :: Parser Term
parseTerm = (nTApp <$> many1 go) <?> "term"
  where
    -- terms without top-level application
    go :: Parser Term
    go = msum [
          uncurry TLam <$> goLam
        , uncurry TPat <$> goPat
        , TCon <$> parseCon <*> many parseTerm
        , TVar <$> parseVar
        , TPtr <$> parsePtr
        , parens parseTerm
        ]

    goLam :: Parser (Var, Term)
    goLam = (,) <$  reservedOp "\\"
                <*> parseVar
                <*  reservedOp "->"
                <*> parseTerm

    goPat :: Parser (Term, [Match])
    goPat = (,) <$  reserved "case"
                <*> parseTerm
                <*  reserved "of"
                <*> braces (parseMatch `sepBy` reservedOp ";")

{-------------------------------------------------------------------------------
  Lexical analysis
-------------------------------------------------------------------------------}

lexer = P.makeTokenParser haskellDef {
      P.reservedNames   = ["case", "of"]
    , P.reservedOpNames = ["\\", "->", ";", "@"]
    }

braces     = P.braces     lexer
identifier = P.identifier lexer
integer    = P.integer    lexer
lexeme     = P.lexeme     lexer
parens     = P.parens     lexer
reserved   = P.reserved   lexer
reservedOp = P.reservedOp lexer
whiteSpace = P.whiteSpace lexer

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

parseTopLevel :: Parser a -> Parser a
parseTopLevel p = whiteSpace *> p <* eof

parseIO :: Parser a -> String -> IO a
parseIO p str =
  case parse p "" str of
    Left err -> throwIO (userError (show err))
    Right a  -> return a

location :: Q SourcePos
location = aux <$> TH.location
  where
    aux :: TH.Loc -> SourcePos
    aux loc = uncurry (newPos (TH.loc_filename loc)) (TH.loc_start loc)
