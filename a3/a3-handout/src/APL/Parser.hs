{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

lStringLiteral :: Parser String
lStringLiteral = lexeme $ do
  void $ lString "\""
  content <- many (satisfy (/= '"'))
  void $ lString "\""
  return content

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pFunctionApp :: Parser Exp
pFunctionApp = do
  func <- pAtom
  args <- many pAtom
  return $ foldl Apply func args

pPrint :: Parser Exp
pPrint = do
  lKeyword "print"
  str <- lStringLiteral
  expr <- pAtom
  return $ Print str expr

pPut :: Parser Exp
pPut = do
  lKeyword "put"
  key <- pAtom
  value <- pAtom
  return $ KvPut key value

pGet :: Parser Exp
pGet = do
  lKeyword "get"
  expr <- pAtom
  return $ KvGet expr

pLExp :: Parser Exp
pLExp = choice
    [ pPrint,
      pPut,
      pGet,
      If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      do
        lString "\\"
        vname <- lVName
        lString "->"
        exp1 <- pExp
        return $ Lambda vname exp1,
      do
        lKeyword "let"
        vname <- lVName
        lString "="
        exp1 <- pExp
        lKeyword "in"
        exp2 <- pExp
        return $ Let vname exp1 exp2,
      do
        lKeyword "try"
        exp1 <- pExp
        lKeyword "catch"
        exp2 <- pExp
        return $ TryCatch exp1 exp2,
      pFunctionApp
    ]

pExp2 :: Parser Exp
pExp2 = do
  base <- pLExp
  rest base
  where
    rest x =
      choice
        [ do
            lString "**"
            y <- pExp2 
            return $ Pow x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp3 :: Parser Exp
pExp3 = pExp0 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp0
            chain $ Eql x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp3

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x  -> Right x
