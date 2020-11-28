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

lineFeed = C.string "\\n" *> pure ELineFeed
tab = C.string "\\t" *> pure ETab
carret = C.string "\\r" *> pure ECarriageReturn
zero = C.string "\\0" *> pure EZero
backslash = C.string "\\\\" *> pure EBackslash
quote = C.string "\\'" *> pure EQuote
dquote = C.string "\\\"" *> pure EDoubleQuote

asciicode :: Parser EscapeSequence
asciicode = C.string "\\x" *> do
                x <- C.hexDigitChar
                y <- C.hexDigitChar
                pure (EAsciiCode (digitToInt x) (digitToInt y))

escapeSequence :: Parser Character
escapeSequence = fmap CEscapeSequence $ lineFeed <|> tab <|> carret <|> zero <|> backslash <|> quote <|> dquote

usualChar :: Parser Character
usualChar = fmap CChar $ notFollowedBy (C.char '\\' <|> C.char '\'' <|> C.char '"') *> C.printChar
    
character = escapeSequence <|> usualChar

rws :: [Text]
rws = ["def", "decl", "if", "elif", "else", "var", "ref", "as", "is", "true", "false", "break", "return", "exit", "loop", "skip", "continue", "int", "byte", "not", "and", "or", "begin", "end"]

identifier :: Parser Text
identifier = (lexeme . try) $ region (setErrorOffset 0) $ (p >>= check)
    where p = fmap pack $ (:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_')
          check x = if x `elem` rws
                       then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                       else return x

withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan p = do
  p1 <- getSourcePos
  x  <- p
  p2 <- getSourcePos
  return (x, SS p1 p2)

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SS p11 p12) (SS p21 p22) = SS p11 p22

annotate = withSpan

labelIdentifier :: Parser (LabelIdentifier ParserAst)
labelIdentifier = annotate $ 
    do i <- identifier
       pure $ LabelIdentifier i

varIdentifier :: Parser (VarIdentifier ParserAst)
varIdentifier = annotate $ 
    do i <- identifier
       pure $ VarIdentifier i

funcIdentifier :: Parser (FuncIdentifier ParserAst)
funcIdentifier = annotate $ 
    do i <- identifier
       pure $ FuncIdentifier i

{-
charConst :: Parser (CharConst ParserAst)
charConst = annotate $ 
    do c <- quotes character
       pure $ CharConst c

stringLiteral :: Parser (StringLiteral ParserAst)
stringLiteral = annotate $ 
    do s <- dquotes (many character)
       pure $ StringLiteral s

intConst :: Parser (IntConst ParserAst)
intConst = annotate $ 
    do i <- lexeme L.decimal
       pure $ IntConst i

dataType :: Parser DataType 
dataType = (symbol "int" *> pure Integ) <|> (symbol "byte" *> pure Byte)

objectType :: Parser ObjectType
objectType = do
    d <- dataType
    is <- many (brackets intConst)
    pure $ go d $ Prelude.reverse is
        where go d [] = DType d
              go d ((IntConst n,_) : ns) = AType (go d ns) n
    
stype :: Parser (Type ParserAst)
stype = annotate $ 
    try (do otype <- objectType
            symbol "["
            symbol "]"
            pure $ PType otype)
    <|> 
    (do otype <- objectType
        pure $ OType otype)
    

fparType :: Parser (ParPassType ParserAst)
fparType = annotate $ 
    (do symbol "ref" 
        t <- stype 
        pure $ ByRef t) <|>
    (do t <- stype
        pure $ ByDefault t)
           
fparDefs :: Parser [FparDef ParserAst]
fparDefs = do
        is <- some (annotate varIdentifier)
        symbol "as" 
        f <- fparType
        pure $ [(FparDef i f, span) | (i, span) <- is]

header :: Parser (Header ParserAst)
header = annotate $ 
    do i <- funcIdentifier 
       d <- optional (symbol "is" *> dataType)
       defs <- optional (do symbol ":" 
                            fdfs <- sepBy1 fparDefs (symbol ",")
                            pure $ join fdfs)
       case defs of
            Nothing -> pure $ Header i d []
            Just xs -> pure $ Header i d xs

funcDecl :: Parser (FuncDecl ParserAst)
funcDecl = annotate $ 
    do symbol "decl"
       h <- header
       pure $ FuncDecl h

varDefs :: Parser [VarDef ParserAst]
varDefs = do
       symbol "var"
       ids <- some (annotate varIdentifier)
       symbol "is"
       t <- objectType
       pure $ [(VarDef id t, span) | (id, span) <- ids]

lvalueHead :: Parser (Lvalue ParserAst)
lvalueHead = annotate $
    (do id <- varIdentifier
        pure $ LvalueId id) <|>
    (do s <- stringLiteral
        pure $ LvalueStr s) 

lvalue :: Parser (Lvalue ParserAst)
lvalue = do l <- lvalueHead
            exprs <- many (brackets expr)
            pure $ toLvalue l exprs
    where toLvalue l [] = l
          toLvalue l (e : es) = toLvalue (LvalueAx l e, mergeSpans (snd l) (snd e)) es


addOp :: Operator Parser (Expr ParserAst)
addOp = InfixL $ symbol "+" *> pure (\e1 e2 -> (e1 :+ e2, mergeSpans (snd e1) (snd e2)))

mulOp :: Operator Parser (Expr ParserAst)
mulOp = InfixL $ symbol "*" *> pure (\e1 e2 -> (e1 :* e2, mergeSpans (snd e1) (snd e2)))

subOp :: Operator Parser (Expr ParserAst)
subOp = InfixL $ symbol "-" *> pure (\e1 e2 -> (e1 :- e2, mergeSpans (snd e1) (snd e2)))

divOp :: Operator Parser (Expr ParserAst)
divOp = InfixL $ symbol "/" *> pure (\e1 e2 -> (e1 :/ e2, mergeSpans (snd e1) (snd e2)))

modOp :: Operator Parser (Expr ParserAst)
modOp = InfixL $ symbol "%" *> pure (\e1 e2 -> (e1 :% e2, mergeSpans (snd e1) (snd e2)))

orOp :: Operator Parser (Expr ParserAst)
orOp = InfixL $ symbol "|" *> pure (\e1 e2 -> (e1 :| e2, mergeSpans (snd e1) (snd e2)))

andOp :: Operator Parser (Expr ParserAst)
andOp = InfixL $ symbol "&" *> pure (\e1 e2 -> (e1 :& e2, mergeSpans (snd e1) (snd e2)))

minusOp :: Operator Parser (Expr ParserAst)
minusOp = Prefix $ do
    (_, a) <- withSpan $ symbol "-" 
    pure (\e -> (ExprSigned Minus e, mergeSpans a (snd e)))

plusOp :: Operator Parser (Expr ParserAst)
plusOp = Prefix $ do
    (_, a) <- withSpan $ symbol "+"
    pure (\e -> (ExprSigned Plus e, mergeSpans a (snd e)))

notOp :: Operator Parser (Expr ParserAst)
notOp = Prefix $ do
    (_, a) <- withSpan $ symbol "!"
    pure (\e -> (ExprNot e, mergeSpans a (snd e)))


eoperators = [[notOp, plusOp, minusOp],
             [mulOp, divOp, modOp, andOp],
             [addOp, subOp, orOp]]

exprTerm :: Parser (Expr ParserAst)
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

expr :: Parser (Expr ParserAst)
expr = makeExprParser exprTerm eoperators

funcCall :: Parser (FuncCall ParserAst)
funcCall = annotate $ 
    do id <- funcIdentifier
       es <- parens (sepBy expr (symbol ","))
       pure $ FuncCall id es

cnotOp :: Operator Parser (Cond ParserAst)
cnotOp = Prefix $ 
    do (_, a) <- withSpan $ symbol "not" 
       pure (\c -> (CondNot c, mergeSpans a (snd c)))

corOp :: Operator Parser (Cond ParserAst)
corOp = InfixL $ symbol "or" *> pure (\c1 c2 -> (c1 `Or` c2, mergeSpans (snd c1) (snd c2)))

candOp :: Operator Parser (Cond ParserAst)
candOp = InfixL $ symbol "and" *> pure (\c1 c2 -> (c1 `And` c2, mergeSpans (snd c1) (snd c2)))

coperators = [[cnotOp], [candOp], [corOp]]

condTerm :: Parser (Cond ParserAst)
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

cond :: Parser (Cond ParserAst)
cond = makeExprParser condTerm coperators

procCall :: Parser (FuncCall ParserAst)
procCall = annotate $ do
    i <- funcIdentifier
    symbol ":"
    es <- sepBy expr (symbol ",")
    pure $ FuncCall i es

stmt :: Parser (Stmt ParserAst)
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
                    
                   
mblock :: Parser (Block ParserAst)
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

ablock :: Pos -> Parser (Block ParserAst)
ablock ref = annotate $
    do s <- blocked1 ref stmt
       pure $ Block s

block :: Pos -> Parser (Block ParserAst)
block ref = mblock <|> ablock ref

localDefs :: Parser [LocalDef ParserAst]
localDefs = do
    ldfs <- many $ 
                (do (f, sp) <- funcDecl
                    pure [(LocalDefFuncDecl (f,sp), sp)]) <|>
                (do (f, sp) <- funcDef
                    pure [(LocalDefFuncDef (f,sp), sp)]) <|>
                (do vs <- varDefs
                    pure $ fmap (\(x,y) -> (LocalDefVarDef (x,y), y)) vs) 
    pure $ join ldfs

funcDef :: Parser (FuncDef ParserAst)
funcDef = annotate $
    do pos <- L.indentLevel 
       symbol "def"
       h <- header
       l <- localDefs 
       b <- block pos
       pure $ FuncDef h l b
          
ast :: Parser (Ast ParserAst)
ast = annotate $ 
    do f <- funcDef
       eof
       pure $ Ast f
-}
        
        


