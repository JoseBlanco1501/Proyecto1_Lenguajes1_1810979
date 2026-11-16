-- | Módulo: Motor.Reconocedor
-- Descripción: Combinadores monádicos de reconocimiento basados en ReadS.
module Motor.Reconocedor
  ( Parser (..)
  , caracter
  , satisface
  , char
  , cadena
  , muchos
  , alMenosUno
  , (<|>)
  , (+++)
  , espacios
  , token
  , simbolo
  , esCaracterId
  , identificador
  , parseCompleto
  , parsePrimero
  ) where

import Control.Applicative (Alternative (..))
import Data.Char (isSpace)

-- | Newtype que envuelve un reconocedor funcional del tipo ReadS a.
newtype Parser a = Parser { runParser :: ReadS a }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [ (f x, r) | (x, r) <- p s ]

instance Applicative Parser where
  pure x = Parser $ \s -> [(x, s)]
  Parser pf <*> Parser px = Parser $ \s ->
    [ (f x, r2)
    | (f, r1) <- pf s
    , (x, r2) <- px r1
    ]

instance Monad Parser where
  Parser p >>= f = Parser $ \s ->
    concat [ runParser (f x) r | (x, r) <- p s ]

instance Alternative Parser where
  empty = Parser (const [])
  Parser p <|> Parser q = Parser $ \s -> p s ++ q s

-- | Elección determinista (sesgada a la izquierda): si la primera opción tiene éxito, se usa esa.
(+++) :: Parser a -> Parser a -> Parser a
Parser p +++ Parser q = Parser $ \s -> case p s of
  [] -> q s
  rs -> rs

-- | Reconoce un único carácter arbitrario.
caracter :: Parser Char
caracter = Parser $ \s -> case s of
  []     -> []
  (c:cs) -> [(c, cs)]

-- | Reconoce un carácter que cumpla el predicado dado.
satisface :: (Char -> Bool) -> Parser Char
satisface p = do
  c <- caracter
  if p c then pure c else empty

-- | Reconoce exactamente el carácter indicado.
char :: Char -> Parser Char
char c = satisface (== c)

-- | Reconoce una cadena de caracteres fija.
cadena :: String -> Parser String
cadena []     = pure []
cadena (c:cs) = char c >> cadena cs >> pure (c:cs)

-- | Reconoce cero o más repeticiones de un reconocedor.
muchos :: Parser a -> Parser [a]
muchos p = alMenosUno p +++ pure []

-- | Reconoce una o más repeticiones de un reconocedor.
alMenosUno :: Parser a -> Parser [a]
alMenosUno p = do
  x  <- p
  xs <- muchos p
  pure (x : xs)

-- | Consume cero o más espacios en blanco.
espacios :: Parser String
espacios = muchos (satisface isSpace)

-- | Ejecuta un reconocedor y consume los espacios en blanco posteriores.
token :: Parser a -> Parser a
token p = do
  x <- p
  _ <- espacios
  pure x

-- | Reconoce un símbolo léxico específico descartando espacios posteriores.
simbolo :: String -> Parser String
simbolo = token . cadena

-- | Predicado para caracteres permitidos en identificadores:
-- letras minúsculas (incluyendo acentuadas y eñe), dígitos y guion bajo.
esCaracterId :: Char -> Bool
esCaracterId c =
  (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c == '_'
  || c `elem` "áéíóúñü"

-- | Reconoce un identificador válido sin espacios.
identificador :: Parser String
identificador = alMenosUno (satisface esCaracterId)

-- | Ejecuta un reconocedor requiriendo que se consuma toda la entrada (o solo queden espacios).
parseCompleto :: Parser a -> String -> Maybe a
parseCompleto p s = case [ x | (x, resto) <- runParser p s, all isSpace resto ] of
  (x:_) -> Just x
  []    -> Nothing

-- | Ejecuta un reconocedor y devuelve el primer resultado, sin requerir consumir toda la cadena.
parsePrimero :: Parser a -> String -> Maybe (a, String)
parsePrimero p s = case runParser p s of
  (r:_) -> Just r
  []    -> Nothing
