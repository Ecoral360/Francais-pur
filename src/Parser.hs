module Parser where

import Control.Applicative (Alternative (..), optional)
import Data.List (nub)
import Data.Maybe (fromMaybe)

data ErrorType i
  = EndOfInput -- Expected more input, but there is nothing
  | Unexpected i -- We didn't expect to find this element
  | Expected i i -- We expected to find this element
  | ExpectedEndOfFile i
  | NoChoiceMatched
  | CustomError String -- Extra errors the user may want to create
  | Empty -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

data Error i = Error
  { erOffset :: Offset,
    erError :: ErrorType i
  }
  deriving (Eq, Show)

type Offset = Int

newtype Parser i a = Parser
  { runParser :: [i] -> Offset -> Either [Error i] (Offset, a, [i])
  }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \input offset -> do
    (offset', output, rest) <- p input offset
    pure (offset', f output, rest)

instance Applicative (Parser i) where
  pure a = Parser $ \input offset -> Right (offset, a, input)
  Parser f <*> Parser p = Parser $ \input offset -> do
    (offset', f', rest) <- f input offset
    (offset'', output, rest') <- p rest offset'
    pure (offset'', f' output, rest')

instance Monad (Parser i) where
  return = pure
  Parser p >>= k = Parser $ \input offset -> do
    (offset', output, rest) <- p input offset
    runParser (k output) rest offset'

instance (Eq i) => Alternative (Parser i) where
  empty = Parser $ \_ offset -> Left [Error offset Empty]
  Parser l <|> Parser r = Parser $ \input offset ->
    case l input offset of
      Left err -> case r input offset of
        Left err' -> Left $ nub $ err <> err'
        Right success -> Right success
      Right success -> Right success

token :: (i -> ErrorType i) -> (i -> Bool) -> Parser i i
token mkErr predicate = Parser $ \input offset -> case input of
  [] -> Left [Error offset EndOfInput]
  i : is
    | predicate i -> Right (offset + 1, i, is)
    | otherwise -> Left [Error offset $ mkErr i]

parseError :: ErrorType i -> Parser i a
parseError err = Parser $ \input offset -> Left [Error offset err]

eof :: Parser i ()
eof = Parser $ \input offset -> case input of
  [] -> Right (offset, (), [])
  c : _ -> Left [Error offset $ ExpectedEndOfFile c]

satisfy :: (i -> Bool) -> Parser i i
satisfy = token Unexpected

anyChar :: Parser i i
anyChar = token Unexpected (const True)

char :: (Eq i) => i -> Parser i i
char i = token (Expected i) (== i)

-- string [] = pure []
-- string (c : cs) = (:) <$> char c <*> string cs
string :: (Eq i) => [i] -> Parser i [i]
string = traverse char

choice :: (Eq i) => [Parser i a] -> Parser i a
choice = foldr (<|>) $ parseError NoChoiceMatched

--
-- many, many1 :: (Eq i) => Parser i a -> Parser i [a]
-- many p = many1 p <|> pure []
-- many1 p = (:) <$> p <*> Parser.many p
--
sepBy, sepBy1 :: (Eq i) => Parser i a -> Parser i s -> Parser i [a]
sepBy p s = sepBy1 p s <|> pure []
sepBy1 p s = (:) <$> p <*> many (s >> p)

between :: Parser i a -> Parser i b -> Parser i c -> Parser i c
between open close p = open *> p <* close

collect :: [Parser i [a]] -> Parser i [a]
collect = foldr (\x acc -> (<>) <$> x <*> acc) $ pure []

manyUntil :: (Eq i) => Parser i a -> Parser i b -> Parser i [b]
manyUntil stop repeated =
  choice
    [ [] <$ stop,
      (:) <$> repeated <*> manyUntil stop repeated
    ]

-- |
-- Returns a parser with a default value if it didn't parse
(<|$>) :: (Eq i) => Parser i a -> a -> Parser i a
(<|$>) p val = fromMaybe val <$> optional p
