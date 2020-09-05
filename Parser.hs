{-# LANGUAGE OverloadedStrings #-}

module Parser (parseLyliet) where

import Data.Char
import Data.Void
import Data.Maybe
import Data.Text (Text)
import Control.Monad
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import AST

type Parser = Parsec Void Text

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
decl = Decl <$> t <*> name <*> optional (symbol "=" >> expr) <* symbol ";"

block :: Parser Statement
block = Block <$> between (symbol "{") (symbol "}") (many stmt)

returnStmt :: Parser Statement
returnStmt = symbol "return" >> Return <$> expr <* symbol ";"

breakStmt :: Parser Statement
breakStmt = Break <$ (symbol "break" >> symbol ";")

continue :: Parser Statement
continue = Continue <$ (symbol "continue" >> symbol ";")

stmt :: Parser Statement
stmt = for <|> ifBlock <|> while <|> block <|> returnStmt <|> breakStmt <|> continue <|> try line <|> decl <?> "statement"

-- parsing of expressions
name :: Parser Name
name = (lexeme $ do
  l <- letterChar
  r <- takeWhileP Nothing (liftM2 (||) isAlphaNum (=='_'))
  return $ T.cons l r) <?> "identifier"

commaSplit :: Parser a -> Parser [a]
commaSplit = flip sepEndBy (symbol ",")

nameLit :: Parser Expr
nameLit = Name <$> name

int :: Parser Expr
int = IntLit <$> L.decimal <* sc

charLit :: Parser Expr
charLit = IntLit . fromEnum <$> between (char '\'') (symbol "'") L.charLiteral

list :: Parser Expr
list = ListLit <$> between (symbol "[") (symbol "]") (commaSplit expr)

stringLit :: Parser Expr
stringLit = ListLit . map (IntLit . fromEnum) <$> (char '"' >> manyTill L.charLiteral (char '"'))

call :: Parser Expr
call = Call <$> name <*> parens (commaSplit expr)

term :: Parser Expr
term = parens expr <|> try call <|> stringLit <|> list <|> charLit <|> int <|> nameLit <?> "term"

op n = try $ (symbol n <* notFollowedBy (char '=') <?> "operator")
given t s = t ((\x y -> Call s [x, y]) <$ op s)
il = map (given InfixL)
ir = map (given InfixR)
iN = map (given InfixN)
pf = map (\s -> Prefix (foldr1 (.) <$> some ((Call s . pure) <$ symbol s)))

ops = [ il ["^"]
      , pf ["+", "-"]
      , il ["*", "%"]
      , il ["+", "-"]
      , iN ["==", "<", ">", ">=", "<="]
      , pf ["!"]
      , il ["||", "&&"]
      , ir ["=", "+=", "-=", "*=", "%="] ]

expr :: Parser Expr
expr = makeExprParser term ops <?> "expression"

parseLyliet :: Text -> String -> IO (Maybe [TopLevel])
parseLyliet t f =
  case parse lyliet f t of
    Left b  -> Nothing <$ putStrLn (errorBundlePretty b)
    Right p -> Just p  <$ return ()
