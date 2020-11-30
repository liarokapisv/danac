{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Danac.Parser.Ast where
    
import Danac.Core.Ast
import Text.Megaparsec.Pos
import Data.Text

data PS

data SourceSpan = SS SourcePos SourcePos
    deriving (Eq, Show)

data Located a = Located { value :: a, srcSpan :: SourceSpan }
    deriving (Eq, Show)

type instance IdP PS = Text
type instance CharP PS = Char
type instance StringP PS = Text
type instance IntP PS = Integer

type instance XLabelIdentifier PS = ()
pattern LabelIdentifierPS x = LabelIdentifier () x

type instance XVarIdentifier PS = ()
pattern FuncIdentifierPS x = FuncIdentifier () x

type instance XFuncIdentifier PS = ()
pattern VarIdentifierPS x = VarIdentifier () x

type instance XCharConst PS = ()
pattern CharConstPS x = CharConst () x

type instance XStringLiteral PS = ()
pattern StringLiteralPS x = StringLiteral () x

type instance XIntConst PS = ()
pattern IntConstPS x = IntConst () x

type instance XFuncCall PS = ()
pattern FuncCallPS x y = FuncCall () x y

type instance XLvalue PS = ()
pattern LvalueIdPS x = LvalueId () x
pattern LvalueStrPS x = LvalueStr () x
pattern LvalueAxPS x y = LvalueAx () x y

type instance XExpr PS = ()
pattern ExprIntConstPS x = ExprIntConst () x
pattern ExprCharConstPS x = ExprCharConst () x
pattern ExprLvaluePS x = ExprLvalue () x
pattern ExprParenPS x = ExprParen () x
pattern ExprFuncCallPS x = ExprFuncCall () x
pattern ExprSignedPS x y = ExprSigned () x y
pattern ExprAddPS x y = ExprAdd () x y
pattern ExprSubPS x y = ExprSub () x y
pattern ExprMulPS x y = ExprMul () x y
pattern ExprDivPS x y = ExprDiv () x y
pattern ExprModPS x y = ExprMod () x y
pattern ExprOrPS  x y = ExprOr () x y
pattern ExprAndPS x y = ExprAnd () x y
pattern ExprTruePS = ExprTrue ()
pattern ExprFalsePS = ExprFalse ()
pattern ExprNotPS x = ExprNot () x

type instance XRec PS a = Located a
