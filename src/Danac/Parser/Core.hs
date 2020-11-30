{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Danac.Parser.Core where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Data.Text
import Data.Void
import Data.Char
import Danac.Core.Ast
import Danac.Parser.Ast
import Danac.Parser.Character
import Control.Monad (when, join)

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

lineFeed = C.char '\n'
tab = C.char '\t'
carret = C.char '\r'
zero = C.char '\0'
backslash = C.char '\\'
quote = C.char '\''
dquote = C.char '\"'

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
mergeSpans (SS p11 p12) (SS p21 p22) = SS p11 p22

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

dataType :: Parser DataType 
dataType = (symbol "int" *> pure Integ) <|> (symbol "byte" *> pure Byte)

{-

objectType :: Parser ObjectType
objectType = do
    d <- dataType
    is <- many (brackets intConst)
    pure $ go d $ Prelude.reverse is
        where go d [] = DType d
              go d ((IntConst n,_) : ns) = AType (go d ns) n
    
stype :: Parser (Type PS)
stype = annotate $ 
    try (do otype <- objectType
            symbol "["
            symbol "]"
            pure $ PType otype)
    <|> 
    (do otype <- objectType
        pure $ OType otype)
    

fparType :: Parser (ParPassType PS)
fparType = annotate $ 
    (do symbol "ref" 
        t <- stype 
        pure $ ByRef t) <|>
    (do t <- stype
        pure $ ByDefault t)
           
fparDefs :: Parser [FparDef PS]
fparDefs = do
        is <- some (annotate varIdentifier)
        symbol "as" 
        f <- fparType
        pure $ [(FparDef i f, span) | (i, span) <- is]

header :: Parser (Header PS)
header = annotate $ 
    do i <- funcIdentifier 
       d <- optional (symbol "is" *> dataType)
       defs <- optional (do symbol ":" 
                            fdfs <- sepBy1 fparDefs (symbol ",")
                            pure $ join fdfs)
       case defs of
            Nothing -> pure $ Header i d []
            Just xs -> pure $ Header i d xs

funcDecl :: Parser (FuncDecl PS)
funcDecl = annotate $ 
    do symbol "decl"
       h <- header
       pure $ FuncDecl h

varDefs :: Parser [VarDef PS]
varDefs = do
       symbol "var"
       ids <- some (annotate varIdentifier)
       symbol "is"
       t <- objectType
       pure $ [(VarDef id t, span) | (id, span) <- ids]

lvalueHead :: Parser (Lvalue PS)
lvalueHead = annotate $
    (do id <- varIdentifier
        pure $ LvalueId id) <|>
    (do s <- stringLiteral
        pure $ LvalueStr s) 

lvalue :: Parser (Lvalue PS)
lvalue = do l <- lvalueHead
            exprs <- many (brackets expr)
            pure $ toLvalue l exprs
    where toLvalue l [] = l
          toLvalue l (e : es) = toLvalue (LvalueAx l e, mergeSpans (snd l) (snd e)) es


addOp :: Operator Parser (Expr PS)
addOp = InfixL $ symbol "+" *> pure (\e1 e2 -> (e1 :+ e2, mergeSpans (snd e1) (snd e2)))

mulOp :: Operator Parser (Expr PS)
mulOp = InfixL $ symbol "*" *> pure (\e1 e2 -> (e1 :* e2, mergeSpans (snd e1) (snd e2)))

subOp :: Operator Parser (Expr PS)
subOp = InfixL $ symbol "-" *> pure (\e1 e2 -> (e1 :- e2, mergeSpans (snd e1) (snd e2)))

divOp :: Operator Parser (Expr PS)
divOp = InfixL $ symbol "/" *> pure (\e1 e2 -> (e1 :/ e2, mergeSpans (snd e1) (snd e2)))

modOp :: Operator Parser (Expr PS)
modOp = InfixL $ symbol "%" *> pure (\e1 e2 -> (e1 :% e2, mergeSpans (snd e1) (snd e2)))

orOp :: Operator Parser (Expr PS)
orOp = InfixL $ symbol "|" *> pure (\e1 e2 -> (e1 :| e2, mergeSpans (snd e1) (snd e2)))

andOp :: Operator Parser (Expr PS)
andOp = InfixL $ symbol "&" *> pure (\e1 e2 -> (e1 :& e2, mergeSpans (snd e1) (snd e2)))

minusOp :: Operator Parser (Expr PS)
minusOp = Prefix $ do
    (_, a) <- withSpan $ symbol "-" 
    pure (\e -> (ExprSigned Minus e, mergeSpans a (snd e)))

plusOp :: Operator Parser (Expr PS)
plusOp = Prefix $ do
    (_, a) <- withSpan $ symbol "+"
    pure (\e -> (ExprSigned Plus e, mergeSpans a (snd e)))

notOp :: Operator Parser (Expr PS)
notOp = Prefix $ do
    (_, a) <- withSpan $ symbol "!"
    pure (\e -> (ExprNot e, mergeSpans a (snd e)))


eoperators = [[notOp, plusOp, minusOp],
             [mulOp, divOp, modOp, andOp],
             [addOp, subOp, orOp]]

exprTerm :: Parser (Expr PS)
exprTerm = annotate $ 
    (try $ do f <- funcCall
              pure $ ExprFuncCall f) <|>
    (try $ do l <- lvalue
              pure $ ExprLvalue l) <|>
    (do e <- parens expr
        pure $ ExprParen e) <|>
    (do i <- intConst
        pure $ ExprIntConst i) <|>
    (do c <- charConst
        pure $ ExprCharConst c) <|>
    (symbol "true" *> pure ExprTrue) <|>
    (symbol "false" *> pure ExprFalse)

expr :: Parser (Expr PS)
expr = makeExprParser exprTerm eoperators

funcCall :: Parser (FuncCall PS)
funcCall = annotate $ 
    do id <- funcIdentifier
       es <- parens (sepBy expr (symbol ","))
       pure $ FuncCall id es

cnotOp :: Operator Parser (Cond PS)
cnotOp = Prefix $ 
    do (_, a) <- withSpan $ symbol "not" 
       pure (\c -> (CondNot c, mergeSpans a (snd c)))

corOp :: Operator Parser (Cond PS)
corOp = InfixL $ symbol "or" *> pure (\c1 c2 -> (c1 `Or` c2, mergeSpans (snd c1) (snd c2)))

candOp :: Operator Parser (Cond PS)
candOp = InfixL $ symbol "and" *> pure (\c1 c2 -> (c1 `And` c2, mergeSpans (snd c1) (snd c2)))

coperators = [[cnotOp], [candOp], [corOp]]

condTerm :: Parser (Cond PS)
condTerm = annotate $ 
    (try $ do e1 <- expr
              symbol "="
              e2 <- expr
              pure $ e1 :== e2) <|>
    (try $ do e1 <- expr
              symbol "<>"
              e2 <- expr
              pure $ e1 :<> e2) <|>
    (try $ do e1 <- expr
              symbol "<"
              e2 <- expr
              pure $ e1 :< e2) <|>
    (try $ do e1 <- expr
              symbol ">"
              e2 <- expr
              pure $ e1 :> e2) <|>
    (try $ do e1 <- expr
              symbol "<="
              e2 <- expr
              pure $ e1 :<= e2) <|>
    (try $ do e1 <- expr
              symbol ">="
              e2 <- expr
              pure $ e1 :>= e2) <|>
    (do c <- parens cond
        pure $ CondParen c) <|>
    (do e <- expr
        pure $ CondExpr e) 

cond :: Parser (Cond PS)
cond = makeExprParser condTerm coperators

procCall :: Parser (FuncCall PS)
procCall = annotate $ do
    i <- funcIdentifier
    symbol ":"
    es <- sepBy expr (symbol ",")
    pure $ FuncCall i es

stmt :: Parser (Stmt PS)
stmt = annotate $ 
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
                    
                   
mblock :: Parser (Block PS)
mblock = annotate $ 
    do symbol "begin"
       s <- some stmt
       symbol "end"
       pure $ Block s

indented :: Pos -> Parser a -> Parser (Pos, a)
indented ref p = do pos <- L.indentGuard space GT ref 
                    v <- p
                    pure (pos, v)
        

aligned :: Pos -> Parser a -> Parser a
aligned ref p = L.indentGuard space EQ ref *> p

blocked1 :: Pos -> Parser a -> Parser [a]
blocked1 ref p = do (pos, a) <- indented ref p
                    rest <- many (try $ helper pos)
                    fpos <- getPosition
                    rest' <- traverse (reportErrors pos) rest
                    setPosition fpos
                    pure (a : rest')
    where helper pos' = do pos <- getPosition
                           a <- p
                           when (sourceColumn pos <= ref) $ L.incorrectIndent EQ pos' (sourceColumn pos)
                           pure (pos, a)
          reportErrors ref (pos, v) = setPosition pos *>
            if ref /= sourceColumn pos
               then L.incorrectIndent EQ ref (sourceColumn pos)
               else pure v
                
blocked :: Pos -> Parser a -> Parser [a]
blocked ref p = blocked1 ref p <|> pure []

ablock :: Pos -> Parser (Block PS)
ablock ref = annotate $
    do s <- blocked1 ref stmt
       pure $ Block s

block :: Pos -> Parser (Block PS)
block ref = mblock <|> ablock ref

localDefs :: Parser [LocalDef PS]
localDefs = do
    ldfs <- many $ 
                (do (f, sp) <- funcDecl
                    pure [(LocalDefFuncDecl (f,sp), sp)]) <|>
                (do (f, sp) <- funcDef
                    pure [(LocalDefFuncDef (f,sp), sp)]) <|>
                (do vs <- varDefs
                    pure $ fmap (\(x,y) -> (LocalDefVarDef (x,y), y)) vs) 
    pure $ join ldfs

funcDef :: Parser (FuncDef PS)
funcDef = annotate $
    do pos <- L.indentLevel 
       symbol "def"
       h <- header
       l <- localDefs 
       b <- block pos
       pure $ FuncDef h l b
          
ast :: Parser (Ast PS)
ast = annotate $ 
    do f <- funcDef
       eof
       pure $ Ast f
-}
        
        


