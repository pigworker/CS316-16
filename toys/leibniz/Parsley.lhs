> module Parsley where

> import Data.Char
> import Control.Applicative
> import Control.Monad
> import Control.Monad.State

> (<*^) :: Applicative f => f (a -> b) -> a -> f b
> f <*^ s = f <*> pure s

> newtype P t x = P {runP :: [t] -> Maybe ([t], x, [t])}

> instance Monad (P t) where
>   return x = P $ \ ts -> Just ([], x, ts)
>   P s >>= f = P $ \ts -> do
>     (sts, s', ts) <- s ts
>     (tts, t', ts) <- runP (f s') ts
>     return (sts ++ tts, t', ts)

> parse :: P t x -> [t] -> Maybe x
> parse p ts = case runP p ts of
>   Just (_, x, []) -> Just x
>   _ -> Nothing

> instance Functor (P t) where
>   fmap = ap . return

> instance Applicative (P t) where
>   pure = return
>   (<*>) = ap

> instance Alternative (P t) where
>   empty = P $ \ _ -> Nothing
>   p <|> q = P $ \ ts -> runP p ts <|> runP q ts

> pRest :: P t [t]
> pRest = P $ \ ts -> Just (ts, ts, [])

> pEnd :: P t ()
> pEnd = P $ \ ts -> if null ts then Just ([], (), []) else Nothing

> next :: P t t
> next = P $ \ ts -> case ts of
>   [] -> Nothing
>   (t : ts) -> Just ([t], t, ts)

> pExt :: P t x -> P t ([t], x)
> pExt (P x) = P $ \ ts -> do
>   (xts, x', ts) <- x ts
>   return (xts, (xts, x'), ts)

> pOpt :: P t x -> P t (Maybe x)
> pOpt p = Just <$> p <|> pure Nothing

>{-
> pPlus :: P t x -> P t [x]
> pPlus p = (:) <$> p <*> pStar p

> pStar :: P t x -> P t [x]
> pStar p = pPlus p <|> pure []
> -}

> pSep :: P t s -> P t x -> P t [x]
> pSep s p = (:) <$> p <*> many (s *> p) <|> pure []

> grok :: (a -> Maybe b) -> P t a -> P t b
> grok f p = do
>   a <- p
>   case f a of
>     Just b -> return b
>     Nothing -> empty

> ok :: (a -> Bool) -> a -> Maybe a
> ok p a = guard (p a) >> return a

> tok :: (t -> Bool) -> P t t
> tok p = grok (ok p) next

> teq :: Eq t => t -> P t ()
> teq t = tok (== t) *> pure ()

