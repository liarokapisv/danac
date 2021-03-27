{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module Danac.Parser where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Control.Monad.Combinators.Expr
import Data.Text
import Data.Void
import Data.Char
import Danac.Ast
import Danac.Util.SourceSpan
import Danac.Util.Annotation
import Control.Monad (join)
import Data.Comp.Multi.Term

type Parser = Parsec Void Text

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "//") (L.skipBlockCommentNested "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space 

symbol :: Text -> Parser Text
symbol = L.symbol space

parens = try . between (symbol "(") (symbol ")") 
brackets = try . between (symbol "[") (symbol "]")
dquotes = try . between (symbol "\"") (symbol "\"")
quotes = try . between (symbol "'") (symbol "'")

lineFeed = C.string "\\n" *> pure '\n'
tab = C.string "\\t" *> pure '\t'
carret = C.string "\\r" *> pure '\r'
zero = C.string "\\0" *> pure '\0'
backslash = C.string "\\\\" *> pure '\\'
quote = C.string "\\'" *> pure '\''
dquote = C.string "\\\"" *> pure '\"'

asciicode :: Parser Char
asciicode = C.string "\\x" *> do
                x <- C.hexDigitChar
                y <- C.hexDigitChar
                pure $ chr $ digitToInt x * 16 + digitToInt y

escapeSequence :: Parser Char
escapeSequence = lineFeed <|> tab <|> carret <|> zero <|> backslash <|> quote <|> dquote

usualChar :: Parser Char
usualChar = notFollowedBy (C.char '\\' <|> C.char '\'' <|> C.char '"') *> C.printChar
    
character = escapeSequence <|> usualChar

rws :: [Text]
rws = ["def", "decl", "if", "elif", "else", "var", "ref", "as", "is", "true", "false", "break", "return", "exit", "loop", "skip", "continue", "int", "byte", "not", "and", "or", "begin", "end"]

identifier :: Parser Text
identifier = (lexeme . try) $ region (setErrorOffset 0) $ (p >>= check)
    where p = fmap pack $ (:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_')
          check x = if x `elem` rws
                       then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                       else return x

type Located x = (x, SourceSpan)
type LocatedT = Term (T :&: SourceSpan)

located :: Parser x -> Parser (Located x)
located p = do
  p1 <- getSourcePos
  x  <- p
  p2 <- getSourcePos
  return $ (x, SS p1 p2)

locatedt :: Parser (T LocatedT i) -> Parser (LocatedT i)
locatedt p = do
  p1 <- getSourcePos
  x  <- p
  p2 <- getSourcePos
  return $ (x :&.: SS p1 p2)

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SS p11 _) (SS _ p22) = SS p11 p22

char :: Parser Char
char = quotes character

stringLiteral :: Parser Text
stringLiteral = do
       s <- dquotes (many character)
       pure $ pack s

int :: Parser Integer
int = lexeme L.decimal

dataType :: Parser DataType
dataType = (symbol "int" *> pure Integ) <|> (symbol "byte" *> pure Byte)

stype :: Parser Type
stype = do
    d <- dataType
    is <- many (brackets int)
    pure $ Prelude.foldl AType (DType d) is
    
fancyType :: Parser FancyType
fancyType = 
    try (do symbol "ref" 
            t <- stype 
            pure $ Ref t) <|>
    try (do t <- stype
            symbol "[" 
            symbol "]"
            pure $ Pointer t) <|>
    (do t <- stype
        pure $ Value t)
           
fparDefs :: Parser [LocatedT FparDef]
fparDefs = do
        is <- some $ located identifier
        symbol "as" 
        f <- fancyType
        pure $ [FparDef i f :&.: sp | (i, sp) <- is]

header :: Parser (LocatedT Header)
header = locatedt $ 
    do i <- identifier 
       d <- optional (symbol "is" *> dataType)
       defs <- optional (do symbol ":" 
                            fdfs <- sepBy1 fparDefs (symbol ",")
                            pure $ join fdfs)
       case defs of
            Nothing -> pure $ Header i d []
            Just xs -> pure $ Header i d xs

funcDecl :: Parser (LocatedT FuncDecl)
funcDecl = locatedt $ 
    do symbol "decl"
       h <- header
       pure $ FuncDecl h

varDefs :: Parser [LocatedT VarDef]
varDefs = do
       symbol "var"
       ids <- some $ located identifier
       symbol "is"
       t <- stype
       pure $ [VarDef i t :&.: s | (i, s) <- ids]

varId :: Parser (LocatedT VarIdentifier)
varId = locatedt $ VarIdentifier <$> identifier

funcId :: Parser (LocatedT FuncIdentifier)
funcId = locatedt $ FuncIdentifier <$> identifier

lvalueHead :: Parser (LocatedT Lvalue)
lvalueHead = locatedt $ 
    (LvalueId <$> varId) <|> 
    (LvalueStr <$> stringLiteral)

lvalue :: Parser (LocatedT Lvalue)
lvalue = do l <- lvalueHead
            exprs <- many (brackets expr)
            pure $ toLvalue l exprs
    where toLvalue l [] = l
          toLvalue l (e : es) = 
            toLvalue (LvalueAx l e :&.: mergeSpans (getTAnn l) (getTAnn e)) es

binOp :: Text -> (LocatedT i -> LocatedT i -> T LocatedT i) -> Operator Parser (LocatedT i)
binOp sym con = InfixL $ symbol sym *> pure (\e1 e2 -> con e1 e2 :&.: mergeSpans (getTAnn e1) (getTAnn e2))

unOp :: Text -> (LocatedT i -> T LocatedT i) -> Operator Parser (LocatedT i)
unOp sym con = Prefix $ do
    (_, s) <- located $ symbol sym
    pure $ \e -> con e :&.: mergeSpans s (getTAnn e)

eoperators = [fmap (uncurry unOp) [("!",ExprNot), ("+", getTVal), ("-",ExprMinus)],
              fmap (uncurry binOp) [("*", ExprMul), ("/", ExprDiv), ("%", ExprMod), ("&", ExprAnd)],
              fmap (uncurry binOp) [("+", ExprAdd), ("-", ExprSub), ("|", ExprOr)]]

exprTerm :: Parser (LocatedT Expr)
exprTerm = locatedt $ 
    (try $ ExprFuncCall <$> funcCall) <|>
    (try $ ExprLvalue <$> lvalue) <|>
    (fmap getTVal $ parens expr) <|>
    (ExprInt <$> int) <|>
    (ExprChar <$> char) <|>
    (symbol "true" *> pure ExprTrue) <|>
    (symbol "false" *> pure ExprFalse)

expr :: Parser (LocatedT Expr)
expr = makeExprParser exprTerm eoperators

funcCall :: Parser (LocatedT FuncCall)
funcCall = locatedt $ FuncCall <$> funcId <*> parens (sepBy expr (symbol ","))

coperators = [[unOp "not" CondNot], [binOp "and" CondAnd], [binOp "or" CondOr]]

condTerm :: Parser (LocatedT Cond)
condTerm = locatedt $
    (try $ do e1 <- expr
              symbol "="
              e2 <- expr
              pure $ CondEq e1 e2) <|>
    (try $ do e1 <- expr
              symbol "<>"
              e2 <- expr
              pure $ CondNe e1 e2) <|>
    (try $ do e1 <- expr
              symbol "<"
              e2 <- expr
              pure $ CondLt e1 e2) <|>
    (try $ do e1 <- expr
              symbol ">"
              e2 <- expr
              pure $ CondGt e1 e2) <|>
    (try $ do e1 <- expr
              symbol "<="
              e2 <- expr
              pure $ CondLe e1 e2) <|>
    (try $ do e1 <- expr
              symbol ">="
              e2 <- expr
              pure $ CondGe e1 e2) <|>
    (fmap getTVal $ parens cond) <|>
    (do CondExpr <$> expr)

cond :: Parser (LocatedT Cond)
cond = makeExprParser condTerm coperators

procCall :: Parser (LocatedT FuncCall)
procCall = locatedt $ do
    i <- funcId
    symbol ":"
    es <- sepBy expr (symbol ",")
    pure $ FuncCall i es

ifStmt :: Parser (LocatedT CondStmt)
ifStmt = locatedt $ do
    pos <- L.indentLevel 
    symbol "if"
    c <- cond
    symbol ":"
    b <- block pos
    pure $ CondStmt c b

elifStmt :: Parser (LocatedT CondStmt)
elifStmt = locatedt $ do
    pos <- L.indentLevel 
    symbol "elif"
    c <- cond
    symbol ":"
    b <- block pos
    pure $ CondStmt c b

elseBlock :: Parser (LocatedT Block)
elseBlock = locatedt $ do
    pos <- L.indentLevel 
    symbol "else"
    symbol ":"
    (b :&.: _) <- block pos
    pure b

stmt :: Parser (LocatedT Stmt)
stmt = locatedt $ 
    (try $ do l <- lvalue
              symbol ":="
              e <- expr
              pure $ StmtAssign l e) <|>
    (try $ StmtProcCall <$> procCall) <|>
    (symbol "skip" *> pure StmtSkip) <|>
    (symbol "exit" *> pure StmtExit) <|>
    (symbol "return" *> symbol ":" *> (StmtReturn <$> expr)) <|>
    (symbol "break" *> (StmtBreak <$> optional (symbol ":" *> identifier))) <|>
    (symbol "continue" *> (StmtContinue <$> optional (symbol ":" *> identifier))) <|>
    (StmtIf <$> ifStmt <*> many elifStmt <*> optional elseBlock) <|>
    (do pos <- L.indentLevel
        symbol "loop"
        mi <- optional identifier
        symbol ":"
        b <- block pos
        pure $ StmtLoop mi b)

dblock :: Parser (LocatedT Block)
dblock = locatedt $ do
    symbol "begin"
    s <- some stmt
    symbol "end"
    pure $ Block s

indented :: Pos -> Parser a -> Parser (Pos, a)
indented ref p = do 
    pos <- L.indentGuard space GT ref 
    v <- p
    pure (pos, v)

aligned :: Pos -> Parser a -> Parser a
aligned ref p = L.indentGuard space EQ ref *> p

blocked1 :: Pos -> Parser a -> Parser [a]
blocked1 ref p = do 
    (pos, a) <- indented ref p
    rest <- many $ aligned pos p
    return (a : rest)

iblock :: Pos -> Parser (LocatedT Block)
iblock ref = locatedt $ do
    s <- blocked1 ref stmt
    pure $ Block s

block :: Pos -> Parser (LocatedT Block)
block ref = dblock <|> iblock ref

localDefs :: Parser [LocatedT LocalDef]
localDefs = do
    ldfs <- many localDef
    pure $ join ldfs
    where
        localDef = 
                (do f <- funcDecl
                    pure $ [LocalDefFuncDecl f :&.: getTAnn f]) <|>
                (do f <- funcDef
                    pure $ [LocalDefFuncDef f :&.: getTAnn f]) <|>
                (do vs <- varDefs 
                    pure $ fmap (\v -> (LocalDefVarDef v :&.: getTAnn v)) vs)

funcDef :: Parser (LocatedT FuncDef)
funcDef = locatedt $ do
    pos <- L.indentLevel 
    symbol "def"
    h <- header
    l <- localDefs 
    b <- block pos
    pure $ FuncDef h l b
          
ast :: Parser (LocatedT Ast)
ast = locatedt $ do
    f <- funcDef
    eof
    pure $ Ast f
