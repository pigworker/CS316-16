module Lec08 where

import Prelude hiding (Monoid, Foldable (..), Functor(..))
import Data.Void

{- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}
{-   From previous lecture                  -}
{- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

class Monoid b where
  base :: b
  op :: b -> b -> b

instance Monoid [x] where
  base = []
  op = (++)

instance Monoid Int where
  base = 0
  op = (+)

foldList :: Monoid b => (a -> b) -> [a] -> b
foldList measure [] = base
foldList measure (a : as) =
  op (measure a) (foldList measure as)

foldTree :: Monoid b => (a -> b) -> Tree a -> b
foldTree measure Leaf = base
foldTree measure (Node l a r) =
  op (foldTree measure l)
     (op (measure a) (foldTree measure r))

foldMaybe :: Monoid b => (a -> b) -> Maybe a -> b
foldMaybe measure Nothing = base
foldMaybe measure (Just a) = measure a

{- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}
{-   New material from here on down         -}
{- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

{- Part 1: The types of types, and type constructor classes. -}

{- We now have several examples of 'foldX', where 'X' stands for 'List',
   or 'Tree' or 'Maybe'. We are in a similar situation to that for
   monoids. We could have defined 'baseInt' and 'opInt', and
   'baseList' and 'opList', and so on. But we didn't -- we unified the
   common interface into the 'Monoid' type class, and defined all the
   different versions as instances of it. This allows us to write
   functions (like 'foldList') that are generic in the Monoid that is
   being used to combine all the elements.

   All of the version of 'foldX' that we have written so far have been
   for what we could call "container" types: a list container 'n'
   elements arranged sequentially; a tree contains 'n' elements
   arranged in some tree shape; and Maybe either contains zero
   elements ('Nothing') or contains just one element ('Just a').

   Can we perform a unification for all 'container'-style types just
   as we did for 'Monoid' types? We can, but first it is useful to
   look at the differences are between types like 'Int', 'String',
   'Bool', and the container-like types like lists, 'Tree' and
   'Maybe'. -}

{- Part 1.1: The types of types.

   Just as values in Haskell have types, types themselves have types
   (which historically were called 'kinds'). You can get GHCi to tell
   you the type of a type by using ':k' (the 'k' standing for
   'kind'). For example:

       λ> :k Int
       Int :: *

   This tells us that the type of 'Int' is '*'. The type '*' is the
   "type of types". That is, '*' is the type of things that classify
   values. Another example of a type:

       λ> :k String
       String :: *

   Not everything that can appear in a type expression is a
   type. Let's try with one of our "container" types:

       λ> :k Maybe
       Maybe :: * -> *

   'Maybe' is not a type -- it does not have type '*'. Instead, it has
   type '* -> *', indicating that it takes a type as input, and
   returns a type. 'Maybe' is a function that operates at the type
   level. Things of this type are often called type constructors, or
   type operators. Another example, for lists:

       λ> :k []
       [] :: * -> *

   (Note that the list type constructor is written using square
   brackets with nothing inside. The notation '[Int]' is a type (i.e.,
   has type '*') and can also be written '[] Int', similar to 'Tree
   Int'.)

   Another example is the function type constructor, which has the
   following type:

       λ> :k (->)
       (->) :: * -> * -> *

   The function type constructor has two parameters: the input type
   and the output type. This is reflected in its type '* -> * -> *',
   stating that it takes a type and a type, and yields a type. There
   is potential for confusion between the '->' on the left hand side
   here, and the '->'s on the right hand side. Please remember for now
   to keep these separate! -}

{- Part 1.2: Type constructor classes.

   So what's the point? Types are different to type constructors. The
   former live in '*' and the latter live in '* -> *'. To unify all
   our disparate 'foldX' implements into a single type class, we have
   to write a type class for things of type '* -> *'. This is called
   "a type constructor class". We'll call this type constructor class
   'Foldable', since it captures the idea of containers that can be
   'folded'. -}

class Foldable c where -- c :: * -> *
  foldMap :: Monoid b => (a -> b) -> c a -> b

{- Comparing the type of 'foldMap' here to 'foldList', we can see that
   the only difference is that we have replaced '[a]' with 'c a',
   moving from a particular container (lists) to a general one
   'c'. The same comparison stands with 'foldTree' and 'foldMaybe'.

   We can now tell Haskell that lists, trees and maybes are all
   instances of 'Foldable', copy-and-pasting the 'foldX'
   implementations from above, and doing a little bit of renaming. -}

instance Foldable [] where
  foldMap measure [] = base
  foldMap measure (a : as) =
    op (measure a) (foldMap measure as)

instance Foldable Tree where
  foldMap measure Leaf = base
  foldMap measure (Node l a r) =
    op (foldMap measure l)
       (op (measure a) (foldMap measure r))

instance Foldable Maybe where
  foldMap measure Nothing = base
  foldMap measure (Just a) = measure a

{- The pay-off for doing this extra work to think about 'Foldable' and
   define these instances is that we can now define very general
   functions that perform aggregate operations over *any*
   container-like structure that has a 'Foldable' instance. For
   example, summing the values in a container that contains 'Int's: -}

sum :: Foldable c => c Int -> Int
sum c = foldMap id c

{- Or finding the number of elements stored in a container: -}

size :: Foldable c => c a -> Int
size c = foldMap (\_ -> 1) c

{- Satisfy yourself that you can see what is going on in these two
   examples: what Monoid instance is being used? what role does the
   'measure' function play in both? -}

{- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}
{- Part 2: Containers and Functors -}

{- The 'Foldable' class makes sense for any container type constructors
   where the elements contained within can be 'smooshed' together in
   some way. What other concepts can we define that make sense for all
   container-like type constructors?.

   In Lecture 6, the first higher-order function that we saw was the
   map function for lists. It has this type:

      map :: (a -> b) -> [a] -> [b]

   In Exercise 3, you are asked to define a map operation for Trees,
   which has this type:

      mapTree :: (a -> b) -> Tree a -> Tree b

   These functions both a do similar thing: they take a function 'f',
   some structure containing values of type 'a', and return the *same*
   structure, but this time containing values of type 'b'. We can see
   this graphically. 'map' works on lists:

          [ a1, a2, ..., an ]
            |   |        |
            v   v        v
          [ b1, b2, ..., bn ]

   where b1 == f a1, b2 == f a2, ..., bn = f an.

   Similarly, for trees, we have, for example:

      Node (Node Leaf a1 Leaf) a2 (Node Leaf a3 Leaf)
                      |        |             |
                      v        v             v
      Node (Node Leaf b1 Leaf) b2 (Node Leaf b3 Leaf)

   where, again, b1 == f a1, and so on.

   The important point to see here is that in both cases, mapping does
   not affect the *structure* of the container, only the values stored
   within it. This is an important enough concept that there is a
   special name for it. Type constructors that support an operation
   analogous to 'map' are called "Functors". -}

{- Part 2.1: The 'Functor' type class

   Just as we did for the 'Foldable' type class, we generalise the
   types of 'map' and 'mapTree' by replacing the concrete container
   type constructor (lists or trees) with a variable, and put it in a
   type class 'Functor'. (The name 'Functor' is chosen for historical
   reasons, one might also call it 'Mappable'.) -}

class Functor c where -- c :: * -> *
  fmap :: (a -> b) -> c a -> c b

{- A more natural name for 'fmap' would be 'map', but this was already
   taken for mapping over lists. Nevertheless, we can give our first
   instance of Functor for lists, using the 'map' function: -}

instance Functor [] where
  fmap = map

{- Trees are also an instance of Functor. Note that the *structure* of
   the tree is unchanged by 'fmap', but the values stored within the
   tree are affected. You can see that the structure is unaffected by
   noting that 'Leaf' is taken to 'Leaf', 'Node' is taken to 'Node'
   and the order of the sub-trees is not affected. -}

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l s r) = Node (fmap f l) (f s) (fmap f r)

{- 'Maybe' is also a Functor. Again, the structure is preserved:
   'Nothing' is taken to 'Nothing', and 'Just' is taken to 'Just': -}

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just s) = Just (f s)

{- There are also several examples of functors representing very boring
   containers. 'Maybe' represents containers that may store zero or
   one elements. To get a container that stores exactly one element we
   define a newtype 'I': -}

newtype I x = MkI x deriving Show

{- (This is called 'I' because it is short for the 'Identity
   functor'. Note how this definition is similar to the definition of
   the identity function 'id x = x', except that we have to give a
   constructor name 'MkI'.

   Values of type 'I a' only have one value, so there is only one case
   to do when defining 'fmap': -}

instance Functor I where
  fmap f (MkI s) = MkI (f s)

{- Another very boring container is the container that stores zero
   values. Here we define a newtype 'K' that takes two types, and
   returns a type:

       λ> :k K
       K :: * -> * -> *
-}

newtype K a x = MkK a deriving Show

{- Applying 'K' to a single type, e.g. "K Int" gives a container that
   always holds an Int, and nothing else. It ignores the second type
   given to it. Therefore a value of type "K Int String" holds zero
   Strings. The fact that "K a" holds zero values is shown by the fact
   that its 'fmap' instance does not use 'f' on its right-hand side: -}

instance Functor (K a) where
  fmap f (MkK a) = MkK a

{- (The name "K" comes from the German for "constant". Compare the
    definition of "K" to the definition of "const x y = x" -- "const
    x" is the function that always returns "x".) -}

{- Functions can be seen as "very wide" containers. For any type 's',
   the type 's -> t' contains a value of 't' for every value of
   't'. So we can define a Functor instance for '((->) s)': -}

instance Functor ((->) s) where
  fmap = (.)

{- EXERCISE : Define functions that translate back and forth between the
   types (a,a) and (Bool -> a). This shows that homogeneous pairs can
   also be seen as a kind of function. What about '() -> a'? -}

{- EXERCISE : Can you define a Foldable instance for ((->) s)? -}

{- Let's now look at some non-examples.

   We just saw that for any type 's', a function of type 's -> t' can
   be seen as a container of 't's. What about the other way round? Is
   a function of type 't -> s' also a container of 't's?

   To answer this, we need to define a newtype that lets us swap the
   arguments to (->): -}

newtype FunIn t s = MkFunIn (s -> t)

{- We now try to define a Functor instance for "FunIn t", but we get
   stuck and cannot fill in the 'undefined' part. If we ask GHCi for
   the types involved, we are asked to produce a value of type 'b ->
   t' from 'g :: a -> t' and 'f :: a -> b'. This seems impossible --
   there is no way to make things match up. -}

instance Functor (FunIn t) where
  fmap f (MkFunIn g) = MkFunIn undefined

{- But maybe there is a clever trick we can play? To show that there is
   *no* way we can replace 'undefined' with a definition, we look at
   the case when we have a type with no values. At the top of this
   file, we imported the module 'Data.Void', this contains a
   definition of a data type 'Void'. Getting information about this
   type reveals something interesting:

       λ> :info Void
       data Void 	-- Defined in ‘Data.Void’
       instance [safe] Eq Void -- Defined in ‘Data.Void’
       instance [safe] Ord Void -- Defined in ‘Data.Void’
       instance [safe] Read Void -- Defined in ‘Data.Void’
       instance [safe] Show Void -- Defined in ‘Data.Void’

   The data type 'Void' has no constructors -- and so there are no
   values of type 'Void' (except for the dummy value 'undefined',
   which is in all types). For comparison, if we look at 'Bool', we
   are shown two constructors:

       λ> :info Bool
       data Bool = False | True 	-- Defined in ‘GHC.Types’
       [..]

   Is a type with no values useless? Why bother going to the effort of
   saying that nothing is possible? One reason is that we can use it
   to turn off parts of other datatypes. As a simple example, what are
   the values of type '[Void]'? These are lists with no elements, so
   the only possible value is the empty list -- we have turned off the
   possibility of elements, but left the rest of the data structure.

   For our current concerns, we can use 'Void' to show that there is
   no way to fill in the Functor instance for "FunIn t" (except for
   'undefined').

   Let's assume that we are in the following situation, we have:
      absurd :: Void -> ()
   and
      MkFunIn id :: FunIn Void Void
   and we are trying to get a value for
      fmap absurd (MkFunIn id) :: FunIn Void ()

   Since FunIn only as one constructor, we need to find some
      f :: () -> Void
   so we can answer with "MkFun f :: FunIn Void ()".

   However, no such "f" can exist (unless we use 'undefined' or
   'error' or don't terminate), because it has to map a value '()'
   into a data type with no values. Since 'Void' has no values, there
   is no way to write "f". -}

{- Part 2.2: The Functor laws -}

{- Another non-example of a Functor is the type of functions from a type
   to itself: -}

newtype Fun a = MkFun (a -> a)

{- Unlike the case for 'FunIn', it seems that we can write a Functor
   instance for this type. There is always a value of type 'a -> a'
   for any 'a': the identity function: -}

instance Functor Fun where
  fmap f (MkFun g) = MkFun id

{- This seems to be a valid definition of 'fmap' for the type
   constructor 'Fun'. But somehow it doesn't seem right: if we
   intuitively think of fmap as altering all of the values stored in a
   container whilst maintaining the structure, then it seems odd to
   always return the same answer -- the 'id' function in this
   case.

   This is not analogous to the 'K' functor above -- it that case,
   there were no 'x's stored in the values at all. Here, we could have
   have lots of 'a' values.

   To exclude this kind of dodgy definition, we require that Functor
   instances always obey two equational laws that intuitively state
   that 'fmap' does do modification of values and not structure.

   The laws are:

      1. fmap id c == c

         Mapping the identity function over a container should not
         affect the container or its values at all. This is reasonable
         -- if we do nothing to the values stored in the container,
         then the whole thing should be unaffected.

      2. fmap f (fmap g c) == fmap (f . g) c

         If we map a function 'g' over a container, and then map a
         function 'f' over the result, then that ought to be the same
         as just mapping their composition. Again, this is reasonable:
         if we are leaving the structure of a container untouched,
         then it shouldn't matter how many times we traverse over it
         to alter the values stored in it.

   We can now see that the Functor instance for 'Fun' defined above
   fails the first law. We have:

      fmap id (MkFun g) = MkFun id

   but, to satisfy the first law, the result ought to be 'MkFun g'. -}

{- Part 2.3: "Theorems for free" -}

{- The intuitive separation of containers into structure and values
   stored inside the structure has the following interesting and
   useful consequence. As we have seen, 'fmap' modifies values stored
   in a structure, but not the structure itself. Conversely, functions
   that are generic in the types of elements can affect the structure,
   but not the values stored within. Examples of functions that affect
   the structure include:

   1. reverse :: [a] -> [a]

      'reverse' knows nothing about the elements stored in the list,
      so the only thing it can do is copy, discard or duplicate them
      in the output. We can know this from its type alone.

   2. mirror :: Tree a -> Tree a

      Similarly, 'mirror' knows nothing about the values of type 'a'
      stored in the tree, so all it can do is rearrange the tree.

   Since 'reverse' and 'mirror' can only affect the structure of the
   containers, and 'fmap' only affects the values in a container, it
   is possible to derive the following laws "for free":

       reverse (fmap f xs)
    ==
       fmap f (reverse xs)

   and

       mirror (fmap f t)
    ==
       fmap f (mirror t)

   Note that neither of these depend on what 'reverse' or 'mirror'
   do. Just by looking at their types, it is possible to derive these
   laws. In fact, for any functor 'c' and function 'h' of type:

       h :: c a -> c a

   It is always the case that

       h (fmap f x) == fmap f (h x)
-}
