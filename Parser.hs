{-# LANGUAGE OverloadedStrings #-}

module Parser (parseMaybe, lyliet) where

import Data.Void
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

-- some type definitions
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

-- whitespace-handling boilerplate
sc = L.space space1 (L.skipLineComment "//") empty
lexeme = L.lexeme sc
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- parse the program
lyliet :: Parser [TopLevel]
lyliet = sc >> many toplevel

-- top-level parsing
t :: Parser Type
t = Type <$> name <*> (fromMaybe [] <$> (optional $ parens (commaSplit t)))

def :: Parser TopLevel
def = Def <$> t <*> name <*> parens (commaSplit ((,) <$> t <*> name)) <*> stmt <?> "function definition"

struct :: Parser TopLevel
struct = symbol "struct" >> Struct <$> t <*> between (symbol "{") (symbol "}") (sepEndBy ((,) <$> t <*> name) (symbol ";")) <?> "struct definition"

toplevel :: Parser TopLevel
toplevel = struct <|> def

-- parsing of statements
line :: Parser Statement
line = Line <$> expr <* symbol ";"

ifBlock :: Parser Statement
ifBlock = symbol "if" >> If <$> parens expr <*> stmt <*> optional (symbol "else" >> stmt)

while :: Parser Statement
while = symbol "while" >> While <$> parens expr <*> stmt

for :: Parser Statement
for = do
  symbol "for"
  (n, e) <- parens ((,) <$> (name <* symbol ":") <*> expr)
  b <- stmt
  return $ For n e b

decl :: Parser Statement
decl = try $ (Decl <$> t <*> name <*> optional (symbol "=" >> expr)) <* symbol ";"

block :: Parser Statement
block = Block <$> between (symbol "{") (symbol "}") (many stmt)

returnStmt :: Parser Statement
returnStmt = symbol "return" >> Return <$> expr <* symbol ";"

breakStmt :: Parser Statement
breakStmt = Break <$ (symbol "break" >> symbol ";")

continue :: Parser Statement
continue = Continue <$ (symbol "continue" >> symbol ";")

stmt :: Parser Statement
stmt = for <|> ifBlock <|> while <|> block <|> decl <|> returnStmt <|> breakStmt <|> continue <|> line <?> "statement"

-- parsing of expressions
name :: Parser Name
name = (lexeme $ do
  l <- letterChar
  r <- many (alphaNumChar <|> char '_')
  return $ T.pack (l : r)) <?> "identifier"

commaSplit :: Parser a -> Parser [a]
commaSplit = flip sepEndBy (symbol ",")

nameLit :: Parser Expr
nameLit = Name <$> name

int :: Parser Expr
int = IntLit <$> L.decimal

charLit :: Parser Expr
charLit = IntLit . fromEnum <$> between (char '\'') (symbol "'") L.charLiteral

list :: Parser Expr
list = ListLit <$> between (symbol "[") (symbol "]") (commaSplit expr)

stringLit :: Parser Expr
stringLit = ListLit . map (IntLit . fromEnum) <$> (char '"' >> manyTill L.charLiteral (char '"'))

call :: Parser Expr
call = Call <$> name <*> parens (commaSplit expr)

expr :: Parser Expr
expr = parens expr <|> call <|> stringLit <|> list <|> charLit <|> int <|> nameLit <?> "expression"
