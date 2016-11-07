module Lec14 where

import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Char

{- LECTURE 14 : MORE MONADIC PARSING -}

{- Parsers of things are functions from strings to lists of pairs of
   things and strings: -}

newtype Parser a = MkParser (String -> [(a,String)])



runParser :: Parser a -> String -> [a]
runParser (MkParser p) input = [ a | (a,"") <- p input ]

instance Monad Parser where
  -- return :: a -> Parser a
  return x = MkParser (\input -> [(x,input)])

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  MkParser p >>= f =
    MkParser (\input -> [ (b,rest) | (a,rest0) <- p input
                                   , let MkParser p2 = f a
                                   , (b,rest) <- p2 rest0
                                   ])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = pure f <*> p

instance Applicative Parser where
  --pure :: a -> Parser a
  pure = return

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = pf >>= \f -> pa >>= \a -> return (f a)

instance Alternative Parser where
  -- empty :: Parser a
  empty = MkParser (\input -> empty)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  MkParser p1 <|> MkParser p2 =
    MkParser (\input -> p1 input ++ p2 input)

char :: Parser Char
char = MkParser (\input -> case input of
                             []     -> []
                             (c:cs) -> [(c,cs)])

{--------------------------------------------------------------------}
{- Part 1. Building parsers                                         -}
{--------------------------------------------------------------------}

{- Everything to do with parsers from this point on is done in terms of:

     1. The Monad interface  (return, >>=)
     2. The Applicative interface (pure, <*>)
     3. The Alternative interface (empty, <|>)
     4. 'char'
-}

-- Chars that satisfy some predicate
satisfies :: (Char -> Bool) -> Parser Char
satisfies predicate =
  char >>= \c ->
  if predicate c then return c else empty



-- Ignoring results
ignore :: Parser a -> Parser ()
ignore p = const () <$> p
           -- pure (\a -> ()) <*> p -- Parser (a -> ()) -> Parser a -> Parser ()
           -- p >>= \a -> return ()




-- Whitespace
space :: Parser ()
space = ignore (satisfies isSpace)


-- Literal characters
isChar :: Char -> Parser ()
isChar c = ignore (satisfies (==c))

  -- isChar '('


-- zero or more
many :: Parser a -> Parser [a]
many p =  pure []
      <|> ((:) <$> p) <*> many p

-- fmap f s = pure f <*> s
--          = f <$> s

{- Examples using 'many':

     λ> runParser (many space) ""
     [[]]
     λ> runParser (many space) "    "
     [[(),(),(),()]]
     λ> runParser (many char) "    "
     ["    "]
     λ> runParser (many char) " sdhgsjfhksdh   "
     [" sdhgsjfhksdh   "]
-}

-- one or more
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

{- Examples using 'some'

     λ> runParser (some space) ""
     []
     λ> runParser (some space) " "
     [[()]]
-}


-- literal strings
string :: String -> Parser ()
string s =
  many char >>= \inputString ->
  if inputString == s then return () else empty

-- things separated by other things
sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator thing =
      pure []
  <|> (:) <$> thing
          <*> many (const id <$> separator <*> thing)

  -- 1,2,3
  -- a,b,c

  -- sepBy (isChar ',') char

{-
     λ> runParser (sepBy (isChar ',') char) "1,2,3"
     ["123"]
     λ> runParser (sepBy (isChar ',') char) "1,2,,3"
     []
     λ> runParser (sepBy (isChar ',') char) ",1,2,,3"
     []
     λ> runParser (sepBy (isChar ',') char) ",1,2,3"
     []
     λ> runParser (sepBy (isChar ',') char) "1,2,,,"
     []
     λ> runParser (sepBy (isChar ',') char) "1,2,,"
     ["12,"]
     λ> runParser (sepBy (isChar ',') char) "1,2,,,3"
     ["12,3"]
     λ> runParser (sepBy (isChar ',') (satisfies (/=','))) "1,2,,,3"
     []
     λ> runParser (sepBy (isChar ',') (satisfies (/=','))) "1,2,3,3"
     ["1233"]
     λ> runParser (sepBy (isChar ',') (some (satisfies (/=',')))) "1,2,3,3"
     [["1","2","3","3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (/=',') <|> const ',' <$> string "\\,"))) "1,2,3,3"
     [["1","2","3","3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (/=',') <|> const ',' <$> string "\\,"))) "1,2,3\\,3"
     [["1","2","3\\","3"],["1","2","3,3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (\c -> c/=','||c/='\\') <|> const ',' <$> string "\\,"))) "1,2,3\\,3"
     [["1","2","3\\","3"],["1","2","3\\,3"],["1","2","3,3"],["1","2,3\\","3"],["1","2,3\\,3"],["1","2,3,3"],["1,2","3\\","3"],["1,2","3\\,3"],["1,2","3,3"],["1,2,3\\","3"],["1,2,3\\,3"],["1,2,3,3"]]
     λ> runParser (sepBy (isChar ',') (some (satisfies (\c -> c/=','&&c/='\\') <|> const ',' <$> string "\\,"))) "1,2,3\\,3"
     [["1","2","3,3"]]
-}

-- things surrounded by parentheses
parens :: Parser a -> Parser a
parens p = (\_ a _ -> a) <$> isChar '(' <*> p <*> isChar ')'


{--------------------------------------------------------------------}
{- Part 2. Parsing Expressions                                      -}
{--------------------------------------------------------------------}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

{-
    E ::= n
        | E '+' E

    1+1+1

    (1+1)+1
    1+(1+1)
-}

{- Try to parse via left recursion -}
expr_v1 :: Parser Expr
expr_v1 =  Number                  <$> (const 1 <$> some (satisfies isDigit))
       <|> (\e1 _ e2 -> Add e1 e2) <$> expr_v1 <*> isChar '+' <*> expr_v1

{-
     λ> runParser expr_v1 "1+1"
     [Add (Number 1) (Number 1)*** Exception: stack overflow
-}


{- Fully paren'd version -}
expr_v2 :: Parser Expr
expr_v2 = Number                  <$> (const 1 <$> some (satisfies isDigit))
       <|> parens ((\e1 _ e2 -> Add e1 e2) <$> expr_v2 <*> isChar '+' <*> expr_v2)

{-
     λ> runParser expr_v2 "(1+1)"
     [Add (Number 1) (Number 1)]
     λ> runParser expr_v2 "(1+(1+1))"
     [Add (Number 1) (Add (Number 1) (Number 1))]
     λ> runParser expr_v2 "(1+1+1)"
     []
-}

{- E := B
      | B '+' E

   B := n
      | '(' E ')' -}

{- Version with big expressions, little expressions -}
expr_v3 :: Parser Expr
expr_v3 = base_v3
       <|> Add <$> base_v3 <* isChar '+' <*> expr_v3

-- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
-- (<*)  :: Parser a -> Parser b -> Parser a

base_v3 = Number <$> (const 1 <$> some (satisfies isDigit))
       <|> parens (expr_v3)

{-
     λ> runParser expr_v3 "(1+1+1)"
     [Add (Number 1) (Add (Number 1) (Number 1))]
     λ> runParser expr_v3 "1+1+1"
     [Add (Number 1) (Add (Number 1) (Number 1))]
     λ> runParser expr_v3 "(1+1)+1"
     [Add (Add (Number 1) (Number 1)) (Number 1)]
     λ> runParser expr_v3 "(1+1)+1"
     [Add (Add (Number 1) (Number 1)) (Number 1)]
-}


{--------------------------------------------------------------------}
{- Part 3. Do-notation                                              -}
{--------------------------------------------------------------------}

{- Another way of writing 'satisfies' -}

-- do x <- f; c  ==> f >>= \x -> c

satisfies2 :: (Char -> Char -> Bool) -> Parser Char
satisfies2 predicate =
  do c  <- char
     c2 <- char
     guard (predicate c c2)
     return c
{-
  char >>= \c ->
  char >>= \c2 ->
  if predicate c c2 then return c else empty
-}


-- Every monad supports 'do' notation, including the list monad:

squares :: [Int]
squares = do
  n1 <- [1..10]
  n2 <- [1..10]
  guard (n1 >= 5)
  return (n1 * n2)

--   [ n1 * n2 | n1 <- [1..10], n2 <- [1..10], n1 >= 5 ]


