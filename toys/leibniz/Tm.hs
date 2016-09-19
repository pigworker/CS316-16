module Tm where

import Control.Applicative
import Data.Foldable hiding (foldl, elem)
import Data.Maybe
import Data.Char

import ANSIEscapes
import Block
import Overlay
import Parsley
import HaLay

data Tm
  = F String
  | C String
  | V String
  | Tm :$ Tm
  deriving (Show, Eq)

infixl 7 :$

data TmC
  = Root
  | InF TmC Tm
  | InA Tm TmC

rendZ :: (TmC, Tm) -> [Layout Box]
rendZ (c, t) = x : xs where
  (x, xs) = hit (bgl white) $ rendC c (hit (bgl black) $ render (cpar c) t)
  hit f (x, xs) = (f x, map f xs)

cpar :: TmC -> Bool
cpar (InA _ _) = True
cpar _ = False

rendC :: TmC -> (Layout Box, [Layout Box]) ->  (Layout Box, [Layout Box])
rendC Root       l = l
rendC (InF c a)  l = rendC c (rapply (cpar c) l (render True a))
rendC (InA f c)  l = rendC c (rapply (cpar c) (render False f) l)

render :: Bool -> Tm -> (Layout Box, [Layout Box])
render p (F (' ':x)) = (fgl green (layS x), [])
render p (F x@(c : _)) | not (isAlpha c)
  = (fgl plain (layS "(") `joinH` fgl green (layS x) `joinH` fgl plain (layS ")"), [])
render p (F f) = (fgl green (layS f), [])
render p (C c) = (fgl red (layS c), [])
render p (V v) = (fgl magenta (layS v), [])
render p (F x@(c:_) :$ a :$ b) | not (isAlpha c) =
  rapply p (rapply False (render True a) (fgl green (layS x), [])) (render True b)
render p (F x@(c:_) :$ a) | not (isAlpha c) =
  rapply True (render True a) (fgl green (layS x), [])
render p (f :$ a) = rapply p (render False f) (render True a)

rapply :: Bool  -> (Layout Box, [Layout Box]) -> (Layout Box, [Layout Box])
                -> (Layout Box, [Layout Box])
rapply p (hf, vfs) (ha, vas)
  = (par p (hf `joinH` sp `joinH` ha), map (par p) (va (hf : vfs) (ha : vas)))
  where
    par False l = l
    par True  l@((w, h), _)
      = (fgl plain (layS "(") `joinV` vGap (h - 1)) `joinH` l `joinH`
          (vGap (h - 1) `joinV` fgl plain (layS ")"))
    sp = hGap 1
    vj f a = f `joinV` (hGap 2 `joinH` a)
    va [] _ = []
    va _ [] = []
    va (f : fs) as = merge (cut (map (vj f) as)) (va fs as)
    cut (x@((w, _), _) : ((w', _), _) : _) | w <= w' = [x]
    cut (x : xs) = x : cut xs
    cut [] = []
    merge [] ys = ys
    merge xs [] = xs
    merge (x@((wx, hx), _) : xs) (y@((wy, hy), _) : ys)
      | hx < hy = x : merge xs (y : ys)
      | hx > hy = y : merge (x : xs) ys
      | wx < wy = merge (x : xs) ys
      | otherwise = merge xs (y : ys)

data Match x = Yes x | No | Maybe

instance Applicative Match where
  pure = Yes
  Yes f <*> Yes a = Yes (f a)
  No <*> _ = No
  _ <*> No = No
  _ <*> _ = Maybe

instance Functor Match where
  fmap = (<*>) . pure  

match :: Tm -> Tm -> Match [(String, Tm)]
match (V v)     t                        = Yes [(v, t)]
match (C c)     (C c')      | c == c'    = Yes []
match (F f)     (F f')      | f == f'    = Yes []
match (f :$ a)  (f' :$ a')               = (++) <$> match f f' <*> match a a'
match _         t                        = chead t where
  chead (C _) = No
  chead (F _) = Maybe
  chead (f :$ _) = chead f

type Subst = [(String, Tm)]

subst :: Subst -> Tm -> Tm
subst s (V v) = case lookup v s of
  Just t'  -> t'
  Nothing  -> V v
subst s (f :$ a) = subst s f :$ subst s a
subst s x = x

type Lhs = (Tm, Subst -> Match ())

type Rule = ([Lhs], Lhs, Tm)

rule :: Rule -> (TmC, Tm) -> Maybe (TmC, Tm)
rule ((a, h) : ls, pg, e) (c, t) = case match a t of
  No -> rule (ls, pg, e) (c, t)
  Yes s -> case h s of
    No -> rule (ls, pg, e) (c, t)
    _ -> Nothing    
  _  -> Nothing
rule ([], (p, g), e) (c, t) = case match p t of
  Yes s -> case g s of
    Yes () -> Just (c, subst s e)
    _ -> Nothing
  _ -> Nothing

le :: String -> String -> Subst -> Match ()
le x y s = case (lookup x s, lookup y s) of
  (Just (C x), Just (C y)) | (read x :: Int) <= read y -> Yes ()
                           | otherwise -> No
  _ -> Maybe

owise :: Subst -> Match ()
owise _ = Yes ()

keyComment :: Tok -> Maybe Char
keyComment (Com ('-':'-':' ':'[':c:']':_)) = Just c
keyComment _ = Nothing

mundane :: P Tok Tok
mundane = tok (\ x ->  not (elem x [Sym "=", Sym "|"]) &&
                       isNothing (keyComment x))

guardy :: P Tok ([Tok], [([Tok], [Tok], Char)])
guardy = (,) <$> some mundane <*>
           some ((,,) <$ teq (Sym "|") <*> some mundane
                      <* teq (Sym "=") <*> some mundane
                      <*> grok keyComment next <* spc) <* pRest
         <|> (,) <$> some mundane <*> ((:[]) <$>
               ((,,) [Lid "otherwise"] <$  teq (Sym "=") <*> some mundane
                      <*> grok keyComment next)) <* pRest

guardify :: [Tok] -> [(Char, ([Tok], [Tok], [Tok]))]
guardify l = case parse guardy l of
  Nothing -> []
  Just (l, grcs) -> [(c, (l, g, r)) | (g, r, c) <- grcs]

batchIt :: [[Tok]] -> [(String, [(Char, ([Tok], [Tok], [Tok]))])]
batchIt ((Lid _ : ts) : ls) | elem (Sym "::") ts = batchIt ls
batchIt (l@(B Rnd [Sym f] : ts) : m@(Spc _ : Com _ : _) : ls) =
  glom (guardify (l ++ m)) (batchIt ls) where
  glom c ((g, b) : gbs) | f == g = (g, c ++ b) : gbs
  glom c gcs = (f, c) : gcs
batchIt (l@(Lid f : ts) : m@(Spc _ : Com _ : _) : ls) =
  glom (guardify (l ++ m)) (batchIt ls) where
  glom c ((g, b) : gbs) | f == g = (g, c ++ b) : gbs
  glom c gcs = (f, c) : gcs
batchIt (l : ls) = batchIt ls
batchIt [] = []

fpats :: P Tok Tm
fpats = foldl (:$) <$> (hd <* spc) <*> many (pat <* spc) where
  hd = F <$> (pBr Rnd (grok symbol next) <|> lid)
  pat = C <$> uid
     <|> V <$> lid
     <|> pBr Rnd
          (foldl (:$) <$> (C <$ spc <*> uid <* spc) <*> many (pat <* spc))
     <|> pBr Rnd
          ((\ a f b -> f :$ a :$ b) <$ spc <*> pat <* spc <*>
            (C <$> grok icon next)         <* spc <*> pat <* spc)

guardP :: P Tok (Subst -> Match ())
guardP = owise <$ spc <* teq (Lid "otherwise") <* spc
     <|> le <$ spc <*> lid <* spc <* teq (Sym "<=") <* spc <*> lid <* spc

symbol :: Tok -> Maybe String
symbol (Sym s@(c : _)) | c /= ':' && not (elem s ["=", "|"]) = Just s
symbol _ = Nothing

icon :: Tok -> Maybe String
icon (Sym s@(':' : _)) = Just s
icon _ = Nothing

grotF :: String -> Tm
grotF x = F (' ' : x)

rhs :: [String] -> P Tok Tm
rhs vs = ep where
  ep = spc *> (foldl fap <$> hp <*> many (spc *> hp) <* spc)
  hp = C <$> uid <|> V <$> grok (ok (`elem` vs)) lid <|> F <$> lid
       <|> F <$> pBr Rnd (grok symbol next)
       <|> (:$) <$> (grotF <$> grok symbol next) <*> ep
       <|> grotF <$> grok symbol next
       <|> grok litC next
       <|> pBr Rnd ep
  litC (Lit i) = Just (C i)
  litC _ = Nothing
  fap a (F (' ':x)) = (F x :$ a)
  fap a (F (' ':x) :$ b) = (F x :$ a :$ b)
  fap f a = f :$ a

vars :: Tm -> [String]
vars (V x) = [x]
vars (f :$ a) = vars f ++ vars a
vars _ = []

overlap :: Tm -> (Char, Rule) -> [Lhs]
overlap q (_, (_, (p, g), _)) = case match p q of
  Yes s -> case g s of
    No -> []
    _ -> [(p, g)]
  No -> []
  _ -> [(p, g)]

batchRule :: [(Char, Rule)] -> (Char, ([Tok], [Tok], [Tok])) -> [(Char, Rule)]
batchRule rs (c, (l, g, r)) = case parse fpats l of
  Nothing -> rs
  Just p -> case (parse guardP g, parse (rhs (vars p)) r) of
    (Just g, Just r) -> rs ++ [(c, (rs >>= overlap p, (p, g), r))]
    _ -> rs


mkProgram :: [[Tok]] -> [(Char, Rule)]
mkProgram = (>>= foldl batchRule []) . map snd . batchIt

mkExamples :: [[Tok]] -> [Tm]
mkExamples = foldMap (foldMap return . parse pEx) where
  pEx = (grok (ok (\ x -> case x of {('e':'x':_) -> True ; _ -> False})) lid *>
         spc *> teq (Sym "=") *> spc) *>
        rhs []


program :: [(Char, Rule)]
program =
  [  ('a', ([], (F "append" :$ C "Nil" :$ V "ys", owise), V "ys"))
  ,  ('b', ([], (F "append" :$ (C "Cons" :$ V "x" :$ V "xs") :$ V "ys", owise),
             C "Cons" :$ V "x" :$ (F "append" :$ V "xs" :$ V "ys")))
  ,  ('c', ([], (F "insert" :$ V "x" :$ C "Leaf", owise),
             C "Node" :$ C "Leaf" :$ V "x" :$ C "Leaf"))
  ,  ('d', ([], (F "insert" :$ V "x" :$ (C "Node" :$ V "lt" :$ V "y" :$ V "rt"),
                 le "x" "y"),
             C "Node" :$ (F "insert" :$ V "x" :$ V "lt") :$ V "y" :$ V "rt"))
  ,  ('e', ([(F "insert" :$ V "x" :$ (C "Node" :$ V "lt" :$ V "y" :$ V "rt"),
                 le "x" "y")],
            (F "insert" :$ V "x" :$ (C "Node" :$ V "lt" :$ V "y" :$ V "rt"),
                 owise),
             C "Node" :$ V "lt" :$ V "y" :$ (F "insert" :$ V "x" :$ V "rt")))
  ]

example :: Tm
example =  F "insert" :$ C "5" :$ (F "insert" :$ C "6" :$ (F "insert"
           :$ C "2"
           :$ (C "Node" :$ (C "Node" :$ C "Leaf" :$ C "0" :$ C "Leaf") :$
                    C "1" :$
                     (C "Node" :$ C "Leaf" :$ C "3" :$ C "Leaf"))))
