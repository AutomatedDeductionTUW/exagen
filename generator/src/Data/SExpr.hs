{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.SExpr
  ( Expr(..)
  , formatExpr
  , parseSExprs
  ) where

-- base
import Control.Applicative (empty, (<|>), many)
import Data.Char (isSpace)
import Data.Void (Void)

-- megaparsec
import Text.Megaparsec
  (initialPos, eof, errorBundlePretty, parse, SourcePos(..), Parsec,
   (<?>), takeWhile1P, State(..), PosState(..), updateParserState)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

-- mtl
import Control.Monad.Except (MonadError(..))


{-
-- | SMTLib expressions (Lisp-like S-Expressions)
data SExpr a = SExpr ![Expr a]
  deriving stock (Eq, Show, Functor)

data Expr a
  = Value !a
  | Compound !(SExpr a)
  deriving stock (Eq, Show, Functor)
-}


-- | SMTLib expressions (Lisp-like S-Expressions)
data Expr a
  = Value !a
  | SExpr ![Expr a]
  deriving stock (Eq, Show, Functor)



formatExpr :: (a -> String) -> Expr a -> String
formatExpr formatValue = go
  where
    go (Value x) = formatValue x
    go (SExpr xs) = "(" ++ unwords (map go xs) ++ ")"



type Str = String
type Parser = Parsec Void Str

-- | space consumer, skips whitespace and comments
spc :: Parser ()
spc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment ";"
    blockComment = empty  -- no support for block comments atm


-- | @lexeme p@ parses p and consumes all following whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spc


lexemeNamed :: String -> Parser a -> Parser a
lexemeNamed s p = lexeme p <?> s


-- | Parse symbols (i.e., verbatim strings)
symbol :: Str -> Parser Str
symbol = L.symbol spc


sexpr :: Parser (Expr Str)
sexpr = SExpr <$> (symbol "(" *> many expr <* symbol ")")


value :: Parser (Expr Str)
value = lexemeNamed "expr-value" $ Value <$> takeWhile1P Nothing isValueChar
  where
    isValueChar c = not (isSpace c) && c /= '(' && c /= ')'


expr :: Parser (Expr Str)
expr = sexpr <|> value


setPosition :: SourcePos -> Parser ()
setPosition pos = updateParserState $ \s -> s{statePosState = (statePosState s){ pstateSourcePos = pos }}


parseSExprs'
  :: MonadError String m
  => SourcePos
  -> Str
  -> m [Expr Str]
parseSExprs' pos str =
  case parse p (sourceName pos) str of
    Left err -> throwError $ errorBundlePretty err
    Right x -> return x
  where
    p = do setPosition pos
           spc  -- consume leading spaces
           xs <- many sexpr
           eof
           return xs


parseSExprs
  :: MonadError String m
  => String  -- ^ name of source file
  -> Str    -- ^ text to parse
  -> m [Expr Str]
parseSExprs srcName = parseSExprs' (initialPos srcName)
