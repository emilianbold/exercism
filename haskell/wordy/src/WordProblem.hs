module WordProblem (answer) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)

negNum :: Parser Integer
negNum = char '-' >> num >>= return . (0-)

num :: Parser Integer
num = (many1 digit) >>= return . read

number = try negNum <|> num

plus :: Parser (Integer -> Integer -> Integer)
plus = string " plus " >> return (+)

minus = string " minus " >> return (-)
mul = string " multiplied by " >> return (*)

division :: Parser (Integer -> Integer -> Integer)
division = string " divided by " >> return div 

op :: Parser (Integer -> Integer -> Integer)
op = try plus <|> try minus <|> try mul <|> try division

answer :: String -> Maybe Integer
answer text = result $ parse expression "error" text
    where result (Left _) = Nothing
          result (Right x) = Just x

expression :: Parser Integer
expression = do
    string "What is "
    n1 <- number
    p <- many1 two
    let z = foldl (\n f -> f n) n1 p
    return z

two = do
    f <- op
    n2 <- number 
    return (`f` n2)
