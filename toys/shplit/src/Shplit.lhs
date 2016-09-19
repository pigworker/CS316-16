> module Main where

> import Control.Applicative
> import Control.Monad
> import Data.Char
> import Data.List
> import Data.Traversable hiding (mapM)
> import Control.Monad.State

> data Type
>   = V String
>   | D String [Type]
>   | Tup [Type]
>   | Type :->: Type
>   deriving Show

> tsub :: [(String, Type)] -> Type -> Type
> tsub sb (V x) = case lookup x sb of
>   Just t -> t
>   Nothing -> V x
> tsub sb (D d ts) = D d (map (tsub sb) ts)
> tsub sb (Tup ts) = Tup (map (tsub sb) ts)
> tsub sb (s :->: t) = tsub sb s :->: tsub sb t

> type Data = ((String, (String, [String])), [(String,[(String, Type)])])

> dataLines :: String -> [Data]
> dataLines prog = go 0 ('\n':prog) where
>   go i ('{':'-':s) = go (i + 1) s
>   go i ('-':'}':s) | i > 0 = go (i - 1) s
>   go 0 ('\n':'d':'a':'t':'a':c:s) | isSpace c = snarf s
>   go i (c:s) = go i s
>   go i [] = []
>   snarf s = case parse pData s of
>     Just (_, d, s) -> d : dataLines s
>     Nothing -> dataLines s

> sigLines :: String -> [(String, Type)]
> sigLines prog = go 0 ('\n':prog) where
>   go i ('{':'-':s) = go (i + 1) s
>   go i ('-':'}':s) | i > 0 = go (i - 1) s
>   go 0 ('\n':s) = snarf s
>   go i (c:s) = go i s
>   go i [] = []
>   snarf s = case parse pSig s of
>     Just (_, d, s) -> d : go 0 s
>     Nothing -> go 0 s

> newtype P x = P {parse :: String -> Maybe (String, x, String)}
> instance Monad P where
>   return x = P $ \ s -> Just ("", x, s)
>   P f >>= g = P $ \ s0 -> do
>     (r0, a, s1) <- f s0
>     (r1, b, s2) <- parse (g a) s1
>     return (r0 ++ r1, b, s2)
> instance Applicative P where
>   pure = return
>   (<*>) = ap
> instance Functor P where
>   fmap = (<*>) . pure
> instance Alternative P where
>   empty = P $ \ _ -> Nothing
>   P f <|> P g = P $ \ s -> case f s of
>     Nothing -> g s
>     j -> j

> pex :: P x -> P (String, x)
> pex p = P $ \ s -> case parse p s of
>   Just (r, x, s) -> Just (r, (r, x), s)
>   Nothing -> Nothing

> ch :: (Char -> Maybe a) -> P a
> ch f = P $ \ s -> case s of
>   [] -> Nothing
>   c : s -> (,,) [c] <$> f c <*> pure s

> chok :: (Char -> Bool) -> P Char
> chok p = ch f where
>   f c | p c = Just c
>       | otherwise = Nothing

> chis :: Char -> P ()
> chis c = () <$ chok (c ==)

> chunk :: String -> P ()
> chunk s = () <$ traverse chis s

> dent :: P ()
> dent = () <$ some (chis '\n') <* chis ' ' <* dent
>    <|> () <$ chis ' ' <* dent
>    <|> () <$ chunk "{-" <* bcomment 1 <* dent
>    <|> () <$ chunk "--" <* lcomment True <* dent
>    <|> pure ()

> space :: P ()
> space = () <$ some (chis '\n') <* chis ' ' <* space
>    <|> () <$ chis ' ' <* space
>    <|> pure ()

> bcomment :: Int -> P ()
> bcomment 0 = pure ()
> bcomment i = () <$ chunk "-}" <* bcomment (i - 1)
>          <|> () <$ chunk "{-" <* bcomment (i + 1)
>          <|> () <$ ch Just <* bcomment i

> lcomment :: Bool -> P ()
> lcomment False = () <$ many (chok ('\n' /=))
> lcomment True = P $ \ s -> case s of
>   c : _ | isSymChar c -> Nothing
>   '\n' : _ -> Just ("", (), s)
>   c : s -> do
>     (r, _, s) <- parse (lcomment False) s
>     return (c : r, (), s)

> punc :: String -> P ()
> punc s = () <$ dent <* chunk s <* dent

> isIdChar :: Char -> Bool
> isIdChar c = isAlphaNum c || c == '\''

> isSymChar :: Char -> Bool
> isSymChar c = elem c ":/\\!$?%^&*+-<>~="

> uid :: P String
> uid = (:) <$> chok isUpper <*> many (chok isIdChar)
>   <|> "[]" <$ chis '[' <* dent <* chis ']'

> lid :: P String
> lid = do
>   v <- (:) <$> chok isLower <*> many (chok isIdChar)
>   if elem v ["data", "type", "deriving"] then empty else pure v

> ico :: P String
> ico = (:) <$> chok (':'==) <*> many (chok isSymChar)

> pWeeType :: P Type
> pWeeType = tup <$ punc "(" <* dent <*>
>               ((:) <$> pType <*> many (punc "," *> pType)) <* punc ")"
>     <|> (D "[]" . (:[])) <$ punc "[" <*> pType <* punc "]"
>     <|> V <$> lid
>     <|> D <$> uid <*> pure []
>   where
>     tup [t] = t
>     tup ts = Tup ts

> pMidType :: P Type
> pMidType = D <$> uid <*> many (dent *> pWeeType) <|> pWeeType

> pType :: P Type
> pType = do
>   s <- pMidType
>   (s :->:) <$ punc "->" <*> pType <|> pure s

> pNom :: P String
> pNom = id <$ space <* chunk "{-" <* dent <*> lid <* punc "-}"
>    <|> "" <$ dent

> sep :: P () -> P x -> P [x]
> sep s p = (:) <$> p <*> many (s *> p)

> pData :: P Data
> pData = (,) <$> ((,) <$> pNom <*> ((,) <$> uid <*> many (dent *> lid)))
>             <*  punc "="
>             <*> sep (dent *> chunk "|")
>                  (inf <$> ((,) <$> pNom <*> pMidType) <* dent <*> ico
>                       <* space <*> ((,) <$> pNom <*> pMidType)
>                   <|>
>                   (,) <$ dent <*> uid <*> many ((,) <$> pNom <*> pWeeType))
>   where inf a o b = (o, [a, b])

> pSig :: P (String, Type)
> pSig = (,) <$> lid <* punc "::" <* optional (pType <* punc "=>") <*> pType

> type Gap = (String, ())

> type EPat = (String, Pat)

> data Pat
>   = PV String Bool
>   | PC String [(Gap, EPat)]
>   | PP Gap EPat Gap
>   | PT [(Gap, EPat)] Gap
>   | PI EPat Gap String Gap EPat
>   deriving Show

> epat :: EPat -> String
> epat ("", p) = pat p
> epat (s, _) = s

> pat :: Pat -> String
> pat (PV v b) = v ++ if b then "" else "{-?-}"
> pat (PC c as) = c ++ (as >>= \ ((g, _), e) -> g ++ epat e)
> pat (PP (g, ()) e (g', ())) = "(" ++ g ++ epat e ++ g' ++ ")"
> pat (PT as (g, _)) = "("++ (as >>= \ ((g, _), e) -> g ++ epat e) ++ g ++ ")"
> pat (PI e (g, ()) i (g', ()) e') = epat e ++ g ++ i ++ g' ++ epat e'

> pWeePat :: P Pat
> pWeePat = PV <$> lid <*> (False <$ space <* chunk "{-?-}" <|> pure True)
>       <|> PC <$> uid <*> pure []
>       <|> PP <$ chunk "(" <*> pex dent <*> pex pPat <*> pex dent <* chunk ")"
>       <|> PT <$ chunk "(" <*> pTupPat <*> pex dent <* chunk ")"
>  where
>    pTupPat = (:) <$> ((,) <$> pex dent <*> pex pPat)
>                  <*> some ((,) <$> pex (punc ",") <*> pex pPat)
>          <|> pure []

> pMidPat :: P Pat
> pMidPat = PC <$> uid <*> some ((,) <$> pex dent <*> pex pWeePat)
>       <|> pWeePat

> pPat :: P Pat
> pPat = do
>   p <- pex pMidPat
>   (PI p <$> pex dent <*> ico <*> pex dent <*> pex pPat) <|> pure (snd p)

> type Line = (String, [(Gap, EPat)], (String, ()))

> line :: Line -> String
> line (f, ps, (r, ())) =
>   "\n" ++ f ++ (ps >>= \ ((g, ()), e) -> g ++ epat e) ++ r

> pLine :: P Line
> pLine = (,,) <$> lid <*> many ((,) <$> pex dent <*> pex pWeePat)
>         <*> pex (() <$ punc "=" <* many (chok (not . isSpace) *> dent))

> nomChop :: String -> (String, Int)
> nomChop s =
>   if null n then (s, negate 1) else (reverse t, read (reverse n)) where
>     (n, t) = span isDigit (reverse s)

> growTable :: (String, Int) -> [(String, Int)] -> [(String, Int)]
> growTable (s, i) sis = case lookup s sis of
>   Just j | j >= i -> sis
>   _ -> (s, i) : sis

> nomNumTable :: Pat -> [(String, Int)]
> nomNumTable (PV v False) = []
> nomNumTable (PV v True) = [nomChop v]
> nomNumTable (PC _ as) =
>   foldr (foldr growTable . nomNumTable . snd . snd) [] as
> nomNumTable (PP _ (_, p) _) = nomNumTable p
> nomNumTable (PT ps _) =
>   foldr (foldr growTable . nomNumTable . snd . snd) [] ps
> nomNumTable (PI (_, p) _ _ _ (_, p')) =
>   foldr growTable (nomNumTable p') (nomNumTable p)

> charm :: String -> String -> [(Char, String)]
> charm x y = go (reverse x) (reverse y) where
>   go [] _ = []
>   go [c] [] = [(c, [c])]
>   go [c] s = [(c, reverse s)]
>   go (c : cs) [] = (c, [c]) : go cs []
>   go (c : cs) (h : s) = (c, [h]) : go cs s

> csub :: [(Char, String)] -> Char -> String
> csub css c = case lookup c css of
>   Just s -> s
>   Nothing -> [c]

> type Nommo = StateT [(String, Int)] []

> knockOff :: String -> Nommo String
> knockOff x = do
>   sis <- get
>   case lookup x sis of
>     Just i -> do
>       let j = i + 1
>       put ((x, j) : sis)
>       return (x ++ show j)
>     Nothing -> do
>       put ((x, negate 1) : sis)
>       return x

> mang :: [Data] -> [(Char, String)] -> (String, Type) -> Nommo (Gap, EPat)
> mang ds nm (v, t) = do
>   let w = (if null v then fortran ds t else v) >>= csub nm
>   x <- knockOff w
>   return ((" ", ()), ("", PV x True))

> uPP :: String -> Gap -> EPat -> Gap -> EPat
> uPP e g p@("", _) g' = ("", PP g p g')
> uPP e g p g' = (e, PP g p g')

> uPC :: String -> String -> [(Gap, EPat)] -> EPat
> uPC e c gps | any (null . fst . snd) gps = ("", PC c gps)
> uPC e c gps = (e, PC c gps)

> uPI :: String -> EPat -> Gap -> String -> Gap -> EPat -> EPat
> uPI _ p1@(e1,_) g1 c g2 p2@(e2,_) | null e1 || null e2 =
>   ("", PI p1 g1 c g2 p2)
> uPI e p1 g1 c g2 p2 = (e, PI p1 g1 c g2 p2)

> uPT :: String -> [(Gap, EPat)] -> Gap -> EPat
> uPT e geps g | any (null . fst . snd) geps = ("", PT geps g)
> uPT e geps g = (e, PT geps g)

> iPC :: String -> [(Gap, EPat)] -> Pat
> iPC s@(':':_) [(_, p1), (_, p2)] =
>   PI p1 (" ", ()) s (" ", ()) p2
> iPC s@(':':_) ps = PC ("("++s++")") ps
> iPC c ps = PC c ps

> fortran :: [Data] -> Type -> String
> fortran _ (V x) = x
> fortran ds (D d@(c : _) ts) = 
>   case [ (y, as, cs) | ((y, (d', as)), cs) <- ds, d == d' ] of
>     (y, as, cs) : _ | length as == length ts ->
>       (if null y then [toLower c] else y) >>=
>         csub (concat (zipWith charm as (map (fortran ds) ts)))
>     _ -> [toLower c]
> fortran _ (_ :->: _) = "f"
> fortran _ (Tup []) = "_"
> fortran _ _ = "x"

> munge :: [Data] -> Type -> EPat -> Nommo EPat
> munge ds t@(D d ts) (e, PV x False) =
>   case [ (y, as, cs) | ((y, (d', as)), cs) <- ds, d == d' ] of
>     (y, as, cs) : _ -> do
>       let (x', _) = nomChop x
>       let (y', _) = nomChop (if null y then fortran ds t else y)
>       let nm = charm y' x'
>       (c, vs) <- lift cs
>       ps <- mapM (mang ds nm) vs
>       case ps of
>         [] -> return ("", PC c [])
>         _ -> return ("", PP ("", ()) ("", iPC c ps) ("", ()))
>     _ -> return (e, PV x True)
> munge ds (Tup ts) (e, PV x False) = do
>   ps <- mapM (\ t -> knockOff (fortran ds t)) ts
>   case ps of
>     [] -> return ("", PT [] ("", ()))
>     p : ps -> return ("", PT ((("", ()), ("", PV p True)) :
>                              map (\ p -> ((", ", ()), ("", PV p True))) ps)
>                              ("", ()))
> munge ds t (e, PP g ep g') = do
>   ep' <- munge ds t ep
>   return (uPP e g ep' g')
> munge ds (D d ts) ep@(e, PC c gps) =
>   case [ (y, as, cs) | ((y, (d', as)), cs) <- ds, d == d' ] of
>     (y, as, cs) : _ | length as == length ts -> do
>       let sb = zip as ts
>       case lookup c cs of
>         Just ats | length ats == length gps -> do
>           let tps = zip ats gps
>           gps' <- mapM (\ ((_, t), (g, p)) -> (,) g <$> munge ds t p) tps
>           return (uPC e c gps')
>         _ -> return ep
>     _ -> return ep
> munge ds (Tup ts) ep@(e, PT gps g) | length ts == length gps = do
>   let tps = zip ts gps
>   gps' <- mapM (\ (t, (g, p)) -> (,) g <$> munge ds t p) tps
>   return (uPT e gps' g)
> munge ds (D d ts) ep@(e, PI p1 g1 c g2 p2) =
>   case [ (y, as, cs) | ((y, (d', as)), cs) <- ds, d == d' ] of
>     (y, as, cs) : _ | length as == length ts -> do
>       let sb = zip as ts
>       case lookup c cs of
>         Just [(_, t1),(_,t2)] -> do
>           p1' <- munge ds (tsub sb t1) p1
>           p2' <- munge ds (tsub sb t2) p2
>           return (uPI e p1' g1 c g2 p2')
>         _ -> return ep
>     _ -> return ep
> munge _ _ ep = return ep

> grok :: Type -> [(Gap, EPat)] ->
>         Maybe ([(Gap, (Type, EPat))], [(String, Int)])
> grok _ [] = Just ([], [])
> grok (s :->: t) ((g,ep) : geps) = do
>   (gteps, sis) <- grok t geps
>   Just ((g, (s, ep)):gteps,
>         foldr growTable sis (nomNumTable (snd ep)))
> grok _ _ = Nothing

> mungeLine :: [Data] -> [(String, Type)] -> Line -> [Line]
> mungeLine ds fs l@(f, geps, r) = case lookup f fs of
>   Just t -> case grok t geps of
>     Just (gteps, sis) -> flip evalStateT sis $
>       (,,) f <$> mapM (\ (g, (t, ep)) -> (,) g <$> munge ds t ep) gteps
>              <*> pure r
>     Nothing -> [l]
>   Nothing -> [l]

> split :: [Data] -> [(String, Type)] -> String -> String
> split ds fs prog = tail $ go 0 ('\n' : prog) where
>   go i ('{':'-':s) = "{-" ++ go (i + 1) s
>   go i ('-':'}':s) | i > 0 = "-}" ++ go (i - 1) s
>   go 0 ('\n':s) = snarf s
>   go i (c:s) = c : go i s
>   go i [] = []
>   snarf s = case parse pLine s of
>     Just (_, l, s) -> (mungeLine ds fs l >>= line) ++ go 0 s
>     Nothing -> '\n' : go 0 s

> shplit :: String -> String
> shplit prog
>   = split (dataLines prog ++ lib) (sigLines prog) prog

> lib :: [Data]
> lib = dataLines . concat $
>   [ "data Bool = True | False\n"
>   , "data {-xs-}[] x = [] | {-x-}x : {-xs-}[x]\n"
>   , "data {-xm-}Maybe x = Nothing | Just {-x-}x\n"
>   , "data Either s t = Left s | Right t\n"
>   ]

> main :: IO ()
> main = interact shplit
