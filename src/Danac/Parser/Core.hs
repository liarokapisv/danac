{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Danac.Parser.Core where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Control.Monad.Combinators.Expr
import Data.Text
import Data.Void
import Data.Char
import Danac.Core.Ast
import Danac.Parser.Ast
import Control.Monad (join)

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

located :: Parser a -> Parser (Located a)
located p = do
  p1 <- getSourcePos
  x  <- p
  p2 <- getSourcePos
  return $ Located { value = x, srcSpan = SS p1 p2 }

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SS p11 _) (SS _ p22) = SS p11 p22

labelIdentifier :: Parser (LabelIdentifier PS)
labelIdentifier = located $ LabelIdentifierPS <$> identifier

varIdentifier :: Parser (VarIdentifier PS)
varIdentifier = located $ VarIdentifierPS <$> identifier

funcIdentifier :: Parser (FuncIdentifier PS)
funcIdentifier = located $ FuncIdentifierPS <$> identifier

charConst :: Parser (CharConst PS)
charConst = located $ 
    do c <- quotes character
       pure $ CharConstPS c

stringLiteral :: Parser (StringLiteral PS)
stringLiteral = located $ 
    do s <- dquotes (many character)
       pure $ StringLiteralPS $ pack s

intConst :: Parser (IntConst PS)
intConst = located $ 
    do i <- lexeme L.decimal
       pure $ IntConstPS i

dataType :: Parser (DataType PS)
dataType = located $ (symbol "int" *> pure Integ) <|> (symbol "byte" *> pure Byte)

objectType :: Parser (ObjectType PS)
objectType = do
    d <- dataType
    is <- many (located $ brackets intConst)
    pure $ go d $ Prelude.reverse is
        where go d [] = Located { value = DType d, srcSpan = srcSpan d }
              go d (Located { value = i, srcSpan = iSpan } : ns) = 
                    let newType = go d ns
                        newSpan = srcSpan newType
                    in Located { value = AType newType i, srcSpan = mergeSpans newSpan iSpan }
    
stype :: Parser (Type PS)
stype = located $ 
    try (do otype <- objectType
            symbol "["
            symbol "]"
            pure $ PType otype)
    <|> 
    (do otype <- objectType
        pure $ OType otype)
    

fparType :: Parser (ParPassType PS)
fparType = located $
    (do symbol "ref" 
        t <- stype 
        pure $ ByRef t) <|>
    (do t <- stype
        pure $ ByDefault t)
           
fparDefs :: Parser [FparDef PS]
fparDefs = do
        is <- some varIdentifier
        symbol "as" 
        f <- fparType
        pure $ [Located { value = FparDef i f, srcSpan = sp } | i@Located { srcSpan = sp } <- is]

header :: Parser (Header PS)
header = located $ 
    do i <- funcIdentifier 
       d <- optional (symbol "is" *> dataType)
       defs <- optional (do symbol ":" 
                            fdfs <- sepBy1 fparDefs (symbol ",")
                            pure $ join fdfs)
       case defs of
            Nothing -> pure $ Header i d []
            Just xs -> pure $ Header i d xs

funcDecl :: Parser (FuncDecl PS)
funcDecl = located $ 
    do symbol "decl"
       h <- header
       pure $ FuncDecl h

varDefs :: Parser [VarDef PS]
varDefs = do
       symbol "var"
       ids <- some (varIdentifier)
       symbol "is"
       t <- objectType
       pure $ [Located { value = VarDef i t, srcSpan = s } | i @ Located { srcSpan = s} <- ids]

lvalueHead :: Parser (Lvalue PS)
lvalueHead = located $ 
    (LvalueIdPS <$> varIdentifier) <|> 
    (LvalueStrPS <$> stringLiteral)

lvalue :: Parser (Lvalue PS)
lvalue = do l <- lvalueHead
            exprs <- many (located $ brackets expr)
            pure $ toLvalue l exprs
    where toLvalue l [] = l
          toLvalue l@(Located { srcSpan = lSpan }) (Located { value = e, srcSpan = eSpan } : es) = 
            toLvalue (Located { value = LvalueAxPS l e, srcSpan = mergeSpans lSpan eSpan }) es


addOp :: Operator Parser (Expr PS)
addOp = InfixL $ symbol "+" *> pure (\e1 e2 -> Located { value = ExprAddPS e1 e2, srcSpan = mergeSpans (srcSpan e1) (srcSpan e2) })

subOp :: Operator Parser (Expr PS)
subOp = InfixL $ symbol "-" *> pure (\e1 e2 -> Located { value = ExprSubPS e1 e2, srcSpan = mergeSpans (srcSpan e1) (srcSpan e2) })

mulOp :: Operator Parser (Expr PS)
mulOp = InfixL $ symbol "*" *> pure (\e1 e2 -> Located { value = ExprMulPS e1 e2, srcSpan = mergeSpans (srcSpan e1) (srcSpan e2) })

divOp :: Operator Parser (Expr PS)
divOp = InfixL $ symbol "/" *> pure (\e1 e2 -> Located { value = ExprDivPS e1 e2, srcSpan = mergeSpans (srcSpan e1) (srcSpan e2) })

modOp :: Operator Parser (Expr PS)
modOp = InfixL $ symbol "%" *> pure (\e1 e2 -> Located { value = ExprModPS e1 e2, srcSpan = mergeSpans (srcSpan e1) (srcSpan e2) })

orOp :: Operator Parser (Expr PS)
orOp = InfixL $ symbol "|" *> pure (\e1 e2 -> Located { value = ExprOrPS e1 e2, srcSpan = mergeSpans (srcSpan e1) (srcSpan e2) })

andOp :: Operator Parser (Expr PS)
andOp = InfixL $ symbol "&" *> pure (\e1 e2 -> Located { value = ExprAndPS e1 e2, srcSpan = mergeSpans (srcSpan e1) (srcSpan e2) })

minusOp :: Operator Parser (Expr PS)
minusOp = Prefix $ do
    Located { srcSpan = s } <- located $ symbol "-" 
    pure $ \e -> Located { value = ExprSignedPS Minus e, srcSpan = mergeSpans s (srcSpan e) }

plusOp :: Operator Parser (Expr PS)
plusOp = Prefix $ do
    Located { srcSpan = s } <- located $ symbol "+" 
    pure $ \e -> Located { value = ExprSignedPS Plus e, srcSpan = mergeSpans s (srcSpan e) }

notOp :: Operator Parser (Expr PS)
notOp = Prefix $ do
    Located { srcSpan = s } <- located $ symbol "!" 
    pure $ \e -> Located { value = ExprNotPS e, srcSpan = mergeSpans s (srcSpan e) }

eoperators = [[notOp, plusOp, minusOp],
             [mulOp, divOp, modOp, andOp],
             [addOp, subOp, orOp]]

exprTerm :: Parser (Expr PS)
exprTerm = located $ 
    (try $ do f <- funcCall
              pure $ ExprFuncCallPS f) <|>
    (try $ do l <- lvalue
              pure $ ExprLvaluePS l) <|>
    (do e <- parens expr
        pure $ ExprParenPS e) <|>
    (do i <- intConst
        pure $ ExprIntConstPS i) <|>
    (do c <- charConst
        pure $ ExprCharConstPS c) <|>
    (symbol "true" *> pure ExprTruePS) <|>
    (symbol "false" *> pure ExprFalsePS)


expr :: Parser (Expr PS)
expr = makeExprParser exprTerm eoperators


funcCall :: Parser (FuncCall PS)
funcCall = located $ 
    do i <- funcIdentifier
       es <- parens (sepBy expr (symbol ","))
       pure $ FuncCallPS i es

cnotOp :: Operator Parser (Cond PS)
cnotOp = Prefix $ 
    do Located { srcSpan = s } <- located $ symbol "not" 
       pure $ \c -> Located { value = CondNot c, srcSpan = mergeSpans s (srcSpan c) }

corOp :: Operator Parser (Cond PS)
corOp = InfixL $ symbol "or" *> pure (\c1 c2 -> Located { value = CondOr c1 c2, srcSpan = mergeSpans (srcSpan c1) (srcSpan c2) })

candOp :: Operator Parser (Cond PS)
candOp = InfixL $ symbol "and" *> pure (\c1 c2 -> Located { value = CondAnd c1 c2, srcSpan = mergeSpans (srcSpan c1) (srcSpan c2) })


coperators = [[cnotOp], [candOp], [corOp]]

condTerm :: Parser (Cond PS)
condTerm = located $
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
    (do c <- parens cond
        pure $ CondParen c) <|>
    (do e <- expr
        pure $ CondExpr e)

cond :: Parser (Cond PS)
cond = makeExprParser condTerm coperators

procCall :: Parser (FuncCall PS)
procCall = located $ do
    i <- funcIdentifier
    symbol ":"
    es <- sepBy expr (symbol ",")
    pure $ FuncCallPS i es

stmt :: Parser (Stmt PS)
stmt = located $ 
    (try $ do l <- lvalue
              symbol ":="
              e <- expr
              pure $ StmtAssign l e) <|>
    (try $ do p <- procCall
              pure $ StmtProcCall p) <|>
    (symbol "skip" *> pure StmtSkip) <|>
    (symbol "exit" *> pure StmtExit) <|>
    (do symbol "return"
        symbol ":"
        e <- expr
        pure $ StmtReturn e) <|>
    (do symbol "break"
        i <- optional (symbol ":" *> labelIdentifier)
        pure $ StmtBreak i) <|>
    (do symbol "continue"
        i <- optional (symbol ":" *> labelIdentifier)
        pure $ StmtContinue i) <|>
    (do i <- pif
        eifs <- many pelif
        e <- optional pelse
        pure $ StmtIf i eifs e) <|>
    (do pos <- L.indentLevel
        symbol "loop"
        mi <- optional labelIdentifier
        symbol ":"
        b <- block pos
        pure $ StmtLoop mi b)
    where pif = do pos <- L.indentLevel 
                   symbol "if"
                   c <- cond
                   symbol ":"
                   b <- block pos
                   pure (c, b)
          pelif = do pos <- L.indentLevel
                     symbol "elif"
                     c <- cond
                     symbol ":"
                     b <- block pos
                     pure (c, b)
          pelse = do pos <- L.indentLevel
                     symbol "else"
                     symbol ":"
                     b <- block pos
                     pure b
                    
                   
dblock :: Parser (Block PS)
dblock = located $ do
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

iblock :: Pos -> Parser (Block PS)
iblock ref = located $ do
    s <- blocked1 ref stmt
    pure $ Block s

block :: Pos -> Parser (Block PS)
block ref = dblock <|> iblock ref

localDefs :: Parser [LocalDef PS]
localDefs = do
    ldfs <- many $ 
                (do f <- funcDecl
                    pure [Located { value = LocalDefFuncDecl f, srcSpan = srcSpan f }]) <|>
                (do f <- funcDef
                    pure [Located { value = LocalDefFuncDef f, srcSpan = srcSpan f }]) <|>
                (do vs <- varDefs
                    pure $ fmap (\v -> Located { value = LocalDefVarDef v, srcSpan = srcSpan v}) vs) 
    pure $ join ldfs

funcDef :: Parser (FuncDef PS)
funcDef = located $ do
    pos <- L.indentLevel 
    symbol "def"
    h <- header
    l <- localDefs 
    b <- block pos
    pure $ FuncDef h l b
          
ast :: Parser (Ast PS)
ast = located $ do
    f <- funcDef
    eof
    pure $ Ast f
