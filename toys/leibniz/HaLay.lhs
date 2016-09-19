> {-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
>     MultiParamTypeClasses #-}

> module HaLay where

> import Control.Arrow (first)
> import Control.Applicative
> import Data.Char
> import Data.List
> import Data.Traversable
> import Control.Monad
> import Control.Monad.State
> import Debug.Trace

> import Parsley

------------------------------------------------------------------------------
Glom a file like this
------------------------------------------------------------------------------

> ready :: String -> String -> [[Tok]]
> ready f = map (munge exTyMu) . fst . getLines (Seek NoLay "") [] .
>           tokenize . (,) ((f, 0), 0)

------------------------------------------------------------------------------
Stage 1 : lexing
------------------------------------------------------------------------------

> type Position = (({-file-}String, {-line-}Int), {-col-}Int)

> tokenize :: (Position, String) -> [(Int, Tok)]
> tokenize = unfoldr (runL ((,) <$> lCol <*> tokIn))

> data Tok
>   = Lit String
>   | Ope BK
>   | Clo BK
>   | Uid String
>   | Lid String
>   | KW String
>   | Sym String
>   | Semi
>   | Spc String
>   | Com String
>   | Urk Char
>   | NL (String, Int)    -- file and new line
>   | B BK [Tok]          -- a bracket
>   | L String [[Tok]]    -- some layout
>   | T Tag [Tok]         -- a tagged region
>   deriving (Show, Eq)

> tokOut :: Tok -> String
> tokOut t = case t of
>   Lit s      -> s
>   Ope b      -> ope b
>   Clo b      -> clo b
>   Uid s      -> s
>   Lid s      -> s
>   KW  s      -> s
>   Sym s      -> s
>   Semi       -> ";"
>   Spc s      -> s
>   Com s      -> s
>   Urk c      -> [c]
>   NL (f, l)  | isSuffixOf ".hers" f -> "\n"
>              | otherwise -> "{-# LINE " ++ show l ++ " " ++ show f ++ " #-}\n"
>   B b ts     -> ope b ++ toksOut ts ++ clo b
>   L s tss    -> s ++ tokssOut tss
>   T _ ts     -> toksOut ts

> toksOut :: [Tok] -> String
> toksOut ts = ts >>= tokOut
> tokssOut :: [[Tok]] -> String
> tokssOut tss = tss >>= toksOut

> isSpcT :: Tok -> Bool
> isSpcT (Spc _)  = True
> isSpcT (NL _)   = True
> isSpcT (Com _)  = True
> isSpcT _        = False

> tokIn :: L Tok
> tokIn = Lit <$> ((:) <$> ch '\"' <*> slit)
>     <|> Lit <$> ((:) <$> ch '\'' <*> haha)
>     <|> NL <$ traverse ch "{-# LINE " <*> whur <* traverse ch "\" #-}" <*
>           some (chk isNL cha)
>     <|> Com <$ stol <*> ((++) <$> traverse ch "#" <*>  spa (not . isNL))
>     <|> Sym <$>  sym
>     <|> Com <$> ((++) <$> traverse ch "--" <*>  spa (not . isNL))
>     <|> Com <$> ((++) <$> traverse ch "{-" <*>  comment 1)
>     <|> Ope Rnd <$ ch '('
>     <|> Clo Rnd <$ ch ')'
>     <|> Ope Sqr <$ ch '['
>     <|> Clo Sqr <$ ch ']'
>     <|> Ope Crl <$ ch '{'
>     <|> Clo Crl <$ ch '}'
>     <|> Semi <$ ch ';'
>     <|> Lit <$> ((:) <$> chk isDigit cha <*> spa isDigit)  -- sod FP now
>     <|> Uid <$> ((:) <$> chk isUpper cha <*> spa isIddy)
>     <|> klid <$> ((:) <$> chk (\ b -> isLower b || b == '_') cha <*> spa isIddy)
>     <|> Spc  <$> ((:) <$> chk isHSpace cha <*> spa isHSpace)
>     <|> NL <$ some (chk isNL cha) <*> lFLine
>     <|> Urk <$> cha
>  where
>    slit = (:) <$> ch '\\' <*> ((:) <$> cha <*> slit)
>      <|> return <$> ch '\"'
>      <|> (:) <$> cha <*> slit
>    haha = (:) <$> ch '\\' <*> ((:) <$> cha <*> haha)
>      <|> return <$> ch '\''
>      <|> (:) <$> cha <*> haha
>    klid s = if elem s keywords then KW s else Lid s
>    comment 0 = pure ""
>    comment i = (++) <$> traverse ch "{-" <*> comment (i + 1)
>            <|> (++) <$> traverse ch "-}" <*> comment (i - 1)
>            <|> (++) <$> ((:) <$> ch '\"' <*> slit) <*> comment i
>            <|> (:) <$> cha <*> comment i
>    whur = flip (,) <$> (read <$> some (chk isDigit cha))
>             <* traverse ch " \""
>             <*> spa (not . (=='\"'))

------------------------------------------------------------------------
Stage 2 : Group according to brackets and layout rules
------------------------------------------------------------------------

> data ChunkMode
>   = Lay String Int
>   | Bra BK
>   | NoLay
>   deriving (Show, Eq)

> getChunks :: ChunkMode -> [Tok] -> [(Int, Tok)] -> ([Tok], [(Int, Tok)])
> getChunks m acc its =
>   let (iss, ius) = span gappy its
>       gappy (_, s) = isSpcT s
>       acss = case iss of
>         [] -> acc
>         _  -> reverse (map snd iss) ++ acc
>   in case ius of
>        [] -> (reverse acc, its)
>        ((i, t) : its') -> case (m, t) of
>          (Lay _ j, _) | not (null acc) && i <= j -> (reverse acc, its)
>          (Lay _ _, Semi) -> (reverse acc, its)
>          (Lay k _, KW e) | elem (k, e) layDKillaz -> (reverse acc, its)
>          (Lay _ _, Clo _) -> (reverse acc, its)
>          (Bra b, Clo b') | b == b' -> (reverse acss, its')
>          (m, Ope b) -> case getChunks (Bra b) [] its' of
>            (cs, its) -> getChunks m (B b cs : acss) its
>          (m, KW e) | elem e lakeys -> case getLines (Seek m e) [] its' of
>            (css, its) -> getChunks m ((L e css) : acss) its
>          _ -> getChunks m (t : acss) its'

 getChunks :: ChunkMode -> [Tok] -> [(Int, Tok)] -> ([Tok], [(Int, Tok)])
 getChunks _ acc [] = (reverse acc, [])
 getChunks m acc its@((i, t) : its') = case (m, t) of
   _ | isSpcT t -> getChunks m (t : acc) its'
   (Lay _ j, _) | not (null acc) && i <= j -> (reverse acc, its)
   (Lay _ _, Semi) -> (reverse (t : acc), its')
   (Lay k _, KW e) | elem (k, e) layDKillaz -> (reverse acc, its)
   (Lay _ _, Clo _) -> (reverse acc, its)
   (Bra b, Clo b') | b == b' -> (reverse acc, its')
   (m, Ope b) -> case getChunks (Bra b) [] its' of
     (cs, its) -> getChunks m (B b cs : acc) its
   (m, KW e) | elem e lakeys -> case getLines (Seek m e) [] its' of
     (css, its) -> getChunks m ((L e css) : acc) its
   _ -> getChunks m (t : acc) its'


> data LineMode
>   = Bracing
>   | Seek ChunkMode String
>   | Edge String Int
>   deriving (Show, Eq)

> getLines :: LineMode -> [[Tok]] -> [(Int, Tok)] -> ([[Tok]], [(Int, Tok)])
> getLines m acc its =
>   let (iss, ius) = span gappy its
>       gappy (_, Semi) = True
>       gappy (_, s) = isSpcT s
>       acss = case iss of
>         [] -> acc
>         _  -> reverse (splendid (map snd iss)) ++ acc
>   in case ius of
>        [] -> (reverse acc, its)
>        ((i, t) : its') -> case (m, t) of
>          (Bracing, Clo Crl) -> (reverse ([Clo Crl] :  acss), its')
>          (_, Clo _) -> (reverse acc, its)
>          (Edge k j, _)
>            | i >= j -> case getChunks (Lay k i) [] ius of
>              ([], _) -> (reverse acc, its)
>              (cs, ius) -> getLines (Edge k i) (reverse (splendid cs) ++ acss) ius
>            | otherwise -> (reverse acc, its)
>          (Seek m s, Ope Crl) | properBrace its'
>            -> getLines Bracing ([Ope Crl] : acss) its'
>          (Seek (Lay k j) s, _)
>            | j < i -> getLines (Edge s i) acss ius
>            | otherwise -> (reverse acc, its)
>          (Seek (Bra b) s,_) -> getLines (Edge s i) acss ius
>          (Seek NoLay s, _) -> getLines (Edge s i) acss ius
>          (Bracing, _) -> case getChunks NoLay [] its of
>             ([], _) -> (reverse acc, its)
>             (cs, ius) -> getLines Bracing (cs : acss) ius

> properBrace :: [(Int, Tok)] -> Bool
> properBrace [] = True
> properBrace ((_, Clo Crl) : _) = False
> properBrace ((_, Semi) : _) = True
> properBrace ((_, Sym s) : _) | elem s ["->", "<-", "="] = True
> properBrace (_ : its) = properBrace its


 getLines :: LineMode -> [[Tok]] -> [(Int, Tok)] -> ([[Tok]], [(Int, Tok)])
 getLines _ acc [] = (reverse acc, [])
 getLines m acc ((_, s) : its) | isSpcT s = eat [s] its where
   eat sacc ((_, s) : its) | isSpcT s = eat (s : sacc) its
   eat sacc its = getLines m (reverse (splendid (reverse sacc)) ++ acc) its
 getLines m acc its@((i, t) : its') = case (m, t) of
   (Bracing, Clo Crl) -> (reverse ([Clo Crl] : acc), its')
   (_, Clo _) -> (reverse acc, its)
   (Edge k j, _)
     | i == j -> case getChunks (Lay k i) [] its of
       ([], its) -> (reverse acc, its)
       (cs, its) -> getLines (Edge k i) (reverse (splendid cs) ++ acc) its
     | otherwise -> (reverse acc, its)
   (Seek m s, Ope Crl) -> getLines Bracing ([Ope Crl] : acc) its'
   (Seek (Lay k j) s, _)
     | j < i -> getLines (Edge s i) acc its
     | otherwise -> (reverse acc, its)
   (Seek (Bra b) s,_) -> getLines (Edge s i) acc its
   (Seek NoLay s, _) -> getLines (Edge s i) acc its
   (Bracing, _) -> case getChunks NoLay [] its of
       ([], its) -> (reverse acc, its)
       (cs, its) -> getLines Bracing (cs : acc) its

> layDKillaz :: [(String, String)]
> layDKillaz = [("of", "where"), ("do", "where"), ("let", "in")]

> splendid :: [Tok] -> [[Tok]]
> splendid [] = [[]]
> splendid (NL fl : ts) = case splendid ts of
>   (ss : sss) | all isSpcT ss  -> [] : (NL fl : ss) : sss
>              | otherwise      -> (NL fl : ss) : sss
> splendid (t : ts) = case splendid ts of
>   (us : sss) -> (t : us) : sss

------------------------------------------------------------------------------
Stage 3 : tag regions of interest
------------------------------------------------------------------------------

> data Tag = Ty | Ki | Ex deriving (Show, Eq)

> tender :: Tok -> Bool
> tender (L _ _ ) = True
> tender t = elem t [Semi, Sym "=", Sym ",", Sym "|", KW "in", KW "deriving"]

> tender' :: Tok -> Bool  -- class and instance headers ended by where or eol
> tender' (L _ _ ) = True
> tender' _ = False

> exTyMu :: [Tok] -> Maybe [Tok]
> exTyMu (t : ts)
>   | elem t [KW "class", KW "instance"]
>   = Just $ t : u' : munge exTyMu vs'
>   | elem t [Sym "::"]
>   = Just $ t : u : munge exTyMu vs
>   | elem t [KW "data", KW "newtype"]
>   = Just $ case vs of
>       Sym "=" : vs -> t : u : Sym "=" : oldStyle vs
>       _ -> t : u : munge exTyMu vs
>   | elem t [KW "type"]
>   = Just $ case vs of
>       Sym "=" : vs -> t : u : Sym "=" : [T Ty (munge tyMu vs)]
>       _ -> t : u : munge exTyMu vs
>   where
>     (u, vs) = first (T Ty . munge tyMu) (span (not . tender) ts)
>     (u', vs') = first (T Ty . munge tyMu) (span (not . tender') ts)
> exTyMu _ = Nothing

> oldStyle :: [Tok] -> [Tok]
> oldStyle ts = case parse (pSep (teq (Sym "|")) (many (tok (/= Sym "|")))) ts of
>     Just tss -> intercalate [Sym "|"] (map go tss)
>   where
>     noInfT (Sym (':':_)) = False
>     noInfT _ = True
>     go ts = case span noInfT ts of
>       (as, (t : bs)) -> [T Ty (munge tyMu as), t, T Ty (munge tyMu bs)]
>       _ -> case span isSpcT ts of
>         (ss, t : ts) -> ss ++ t : map ho ts
>         _ -> ts
>     ho s | isSpcT s = s
>     ho (Sym s) = Sym s
>     ho (B Crl ts) = B Crl (munge exTyMu ts)
>     ho t = T Ty (munge tyMu [t])

> tyMu :: [Tok] -> Maybe [Tok]
> tyMu (Sym "::" : ts) = Just $ Sym "::" : u : munge tyMu vs
>   where
>     (u, vs) = first (T Ki . munge kiMu) (span (not . tender) ts)
> tyMu (B Crl us : ts) = Just $
>   B Crl [T Ex (munge exTyMu us)] : munge tyMu ts
> tyMu _ = Nothing

> kiMu :: [Tok] -> Maybe [Tok]
> kiMu (B Crl us : ts) = Just $
>   B Crl [T Ty (munge tyMu us)] : munge kiMu ts
> kiMu (KW "forall" : ts) = case span (/= Sym ".") ts of
>   (ts, us) -> Just $ KW "forall" : T Ty (munge tyMu ts) : munge kiMu us
> kiMu _ = Nothing


------------------------------------------------------------------------------
Parsley for layout
------------------------------------------------------------------------------

> spc :: P Tok ()
> spc = () <$ many (tok isSpcT)

> pNL :: P Tok ()
> pNL = grok h next where
>   h (NL _)  = Just ()
>   h _       = Nothing

> uid :: P Tok String
> uid = grok h next where
>   h (Uid s) = Just s
>   h _ = Nothing

> lid :: P Tok String
> lid = grok h next where
>   h (Lid s) = Just s
>   h _ = Nothing

> infC :: P Tok String
> infC = grok h next where
>   h (Sym (':' : s)) = Just (':' : s)
>   h _ = Nothing

> pBr :: BK -> P Tok x -> P Tok x
> pBr k p = grok pb next where
>   pb (B j cs) | k == j = parse p cs
>   pb _ = Nothing

> pLay :: String -> P [Tok] x -> P Tok x
> pLay k p = grok pb next where
>   pb (L j tss) | k == j = parse p tss
>   pb _ = Nothing

> pTag :: Tag -> P Tok x -> P Tok x
> pTag t p = grok pb next where
>   pb (T u ts) | t == u = parse p ts
>   pb _ = Nothing

------------------------------------------------------------------------------
Mungers
------------------------------------------------------------------------------

> munge :: ([Tok] -> Maybe [Tok]) -> [Tok] -> [Tok]
> munge m ts = case m ts of
>   Just us -> us
>   Nothing -> case ts of
>     [] -> []
>     (B b ss : ts) -> B b (munge m ss) : munge m ts
>     (L k sss : ts) -> L k (map (munge m) sss) : munge m ts
>     (T t ss : ts) -> T t (munge m ss) : munge m ts
>     (t : ts) -> t : munge m ts

> mungeLines :: ([[Tok]] -> Maybe [[Tok]]) -> ([Tok] -> Maybe [Tok]) ->
>               [[Tok]] -> [[Tok]]
> mungeLines ms m tss = case ms tss of
>     Just uss -> uss
>     Nothing -> case tss of
>       [] -> []
>       (ts : tss) -> munge help ts : mungeLines ms m tss
>   where
>     help ts = m ts <|>
>       case ts of
>         (L k sss : ts) -> Just (L k (mungeLines ms m sss) : munge help ts)
>         _ -> Nothing

> dashOut :: [Tok] -> [Tok]
> dashOut ts = [Com ("-- " ++ easy ts)] where
>   easy [] = []
>   easy (B _ _ : _) = []
>   easy (L _ _ : _) = []
>   easy (t : ts) = tokOut t ++ easy ts

> dental :: [[Tok]] -> [Tok]
> dental [] = [NL ("Dunno.lhs", 0)]
> dental (l@(NL _ : _) : (c : _) : _) | not (isSpcT c) = l
> dental (l : ls) = dental ls

> redent :: [Tok] -> [[Tok]] -> [[Tok]]
> redent (NL _ : r) ((NL p : _) : tss) = redent (NL p : r) tss
> redent nl ((NL _ : _) : tss) = redent nl tss
> redent nl (ts : tss) = nl : ts : redent nl tss
> redent nl [] = []

> preamble :: [[Tok]] -> [[Tok]] -> [[Tok]]
> preamble ls [] = ls
> preamble ls ms@(l@(NL _ : _) : (c : _) : _) | not (isSpcT c) =
>   redent l ls ++ ms
> preamble ls (m : ms) = m : preamble ls ms


------------------------------------------------------------------------------
Classifiers, odds and ends
------------------------------------------------------------------------------

> isNL :: Char -> Bool
> isNL b = elem b "\r\n"

> isHSpace :: Char -> Bool
> isHSpace c = isSpace c && not (isNL c)

> isIddy :: Char -> Bool
> isIddy b = isAlphaNum b || elem b "_'"

> isInfy :: Char -> Bool
> isInfy b = elem b "!#$%&*+-,.:/<=>?@\\^|~"

> data BK = Rnd | Sqr | Crl deriving (Show, Eq)

> ope :: BK -> String
> ope Rnd = "("
> ope Sqr = "["
> ope Crl = "{"

> clo :: BK -> String
> clo Rnd = ")"
> clo Sqr = "]"
> clo Crl = "}"

> keywords :: [String]
> keywords = ["module", "import", "type", "data", "newtype", "pattern", "kind",
>             "let", "in", "case", "of", "do", "forall", "class", "instance",
>             "family", "where", "if", "then", "else", "deriving", "hiding"]

> lakeys :: [String]
> lakeys = ["let", "of", "do", "where"]

> width :: [Tok] -> Int
> width ts = case span noNL ts of
>   (ts, []) -> length (ts >>= tokOut)
>   (_, NL _ : ts) -> width ts
>   where noNL (NL _)  = False
>         noNL _       = True

------------------------------------------------------------------------
The lexer monad
------------------------------------------------------------------------

> newtype L x =
>   MkL {runL ::  (Position, String) -> Maybe (x, (Position, String))}

> lFLine :: L (String, Int)
> lFLine = gets $ \ ((fl, _), _) -> fl

> lCol :: L Int
> lCol = gets $ \ ((_, c), _) -> c

Damn you, inconsistent library versions!

> instance Monad L where
>   return x = MkL $ \ ps -> Just (x, ps)
>   MkL f >>= g = MkL $ \ ps -> do
>     (x, ps) <- f ps
>     runL (g x) ps

> instance Applicative L where
>   pure = return
>   (<*>) = ap

> instance Functor L where
>   fmap = (<*>) . return

> instance Alternative L where
>   empty    = MkL $ \ is -> empty
>   p <|> q  = MkL $ \ is -> runL p is <|> runL q is

> instance MonadPlus L where
>   mzero = empty
>   mplus = (<|>)

> instance MonadState (Position, String) L where
>   get = MkL $ \ ps -> Just (ps, ps)
>   put ps = MkL $ \ _ -> Just ((), ps)

> cha :: L Char
> cha = MkL moo where
>   moo (i, []) = Nothing
>   moo ((fl@(f,l),i), c : s)
>     | isNL c     = Just (c, (((f, l + 1), 0), s))
>     | c == '\t'  = if mod i 8 == 7  then Just (' ', ((fl, i + 1), s))
>                                     else Just (' ', ((fl, i + 1), c : s))
>     | otherwise  = Just (c, ((fl, i + 1), s))

> stol :: L ()
> stol = do
>   i <- lCol
>   guard (i == 0)

> chk :: (t -> Bool) -> L t -> L t
> chk p l = do t <- l ; if p t then return t else empty

> ch :: Char -> L Char
> ch c = chk (== c) cha

> spa :: (Char -> Bool) -> L String
> spa p = (:) <$> chk p cha <*> spa p  <|> pure []

> sym :: L String
> sym = MkL $ \ ((fl, i), s) -> case h s of
>   ("", _) -> Nothing
>   ("--", _) -> Nothing
>   (s, t) -> Just (s, ((fl, i + length s), t))
>  where
>   h (s@('-':'}':_)) = ("", s)
>   h (c : s) | isInfy c = first (c :) (h s)
>   h s = ([], s)
