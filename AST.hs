module AST where

import Data.Text (Text)

type Name = Text
data Expr = Call Name [Expr] | IntLit Int | ListLit [Expr] | Name Name deriving Show
data Statement = Line Expr
               | If Expr Statement (Maybe Statement)
               | While Expr Statement
               | For Name Expr Statement
               | Block [Statement]
               | Decl Type Name (Maybe Expr)
               | Return Expr
               | Break
               | Continue
  deriving Show
data Type = Type Name [Type] deriving Show
data TopLevel = Def Type Name [(Type, Name)] Statement | Struct Type [(Type, Name)] deriving Show
