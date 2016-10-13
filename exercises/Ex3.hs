module Ex2 where

{----------------------------------------------------------------------}
{- CS316 (2016/17) EXERCISE 3                                         -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 7th November.
-- There will be a test on this exercise in the lab on that date.
--
-- Your combined score from the submission and the test will be worth
-- 30% of the overall marks for the class (so one mark, below is worth
-- half a percent).
--
-- The test will consists of further requirements issued as updates to
-- this file, and you will need to make changes in response to the new
-- requirements, then commit a new version of the file by the end of
-- the lab session.

{----------------------------------------------------------------------}
{- HIGHER-ORDER PROGRAMMING                                           -}
{----------------------------------------------------------------------}

{- 3.1 Identify yourself. Encode your name instead of Harry's between the
   quotation marks. Your file might get separated from your repository,
   so we'll need this info to give you your mark. -}

myName :: String
myName = map pred "Ibssz!Qbmnfs"

{- 1 mark -}

{----------------------------------------------------------------------}
{- STRUCTURAL RECURSION ON TREES AND LISTS                            -}
{----------------------------------------------------------------------}

{- In the last exercise, we asked you to write functions by recursion on
   lists and trees. These functions only did one thing. For example,
   'concLists' concatenated lists, 'mirror' mirrored trees.

   In this exercise, you will be using and writing so-called "higher
   order" functions that can do many different things, depending on
   the functions that are passed to them as parameters. -}

{- 3.2. Write filter's evil twin that retains the elements of a list
   that fail the test rather than those that pass. Write your function
   using 'filter'. -}

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = undefined

{- 1 mark -}

{- There are two very general ways of iterating over the elements of a
   list, combining all the elements one by one. We can either go
   'right to left' or 'left to right'. We call the first one
   'iterRight' and the second 'iterLeft', and they are defined as
   follows. Many functions on lists can be defined in terms of these
   functions, as you will see below. -}

iterRight :: (a -> b -> b) -> b -> [a] -> b
iterRight f z []     = z
iterRight f z (x:xs) = f x (iterRight f z xs)

iterLeft :: (b -> a -> b) -> b -> [a] -> b
iterLeft f acc []     = acc
iterLeft f acc (x:xs) = iterLeft f (f acc x) xs

{- These functions are sometimes called 'foldr' and 'foldl'. We use
   these names here to avoid confusion with the general 'fold'
   functions introduced in Lectures 7 and 8. -}

{- 3.3 The product function computes the product of (i.e. multiplies
   together) a list of integers. For example:

     product [1,2,3] = 6

   Define this function using 'iterList'. Do not use explicit
   recursion -- if you write 'productFromIterRight' on the right hand
   side of the equals sign here, you are doing it wrong. -}
productFromIterRight :: [Int] -> Int
productFromIterRight = undefined

{- 1 mark -}

{----------------------------------------------------------------------}
{- 3.4 WILL APPEAR IN THE LAB TEST                                   -}
{-                                                                    -}
{- please leave this comment alone until further notice:              -}
{- we'll release an update to it on GitHub at the start of the test   -}
{- 2 marks                                                            -}
{----------------------------------------------------------------------}

{- 3.5 The reverse function reverses a list. For example:

     reverse [1,2,3] = [3,2,1]

   Define a function that does the same job, but written in terms of
   'iterLeft'. Again, if you write 'reverseFromIterLeft' on the right
   hand side of the equals sign here, you are doing it wrong. -}
reverseFromIterLeft :: [a] -> [a]
reverseFromIterLeft = undefined

{- 2 marks -}

{- 3.6 Now define a function that does the same job as 'iterLeft', but
   defined in using 'iterRight'. Again, using explicit recursion is
   not the answer! -}
iterLeftFromIterRight :: (b -> a -> b) -> b -> [a] -> b
iterLeftFromIterRight = undefined

{- 2 marks -}

{- 'iterLeft' and 'iterRight' allow iteration through a list of
   elements, but don't provide access to the list being iterated
   over. The 'recList' function, provided below, does allow access to
   the underlying list. See how the type of the function argument
   includes an extra '[a]' compared to the corresponding part in the
   type of 'iterRight'. -}

recList :: (a -> ([a], b) -> b) -> b -> [a] -> b
recList f z []     = z
recList f z (x:xs) = f x (xs, recList f z xs)

{- 3.7 Define the insertion into an ordered list function, as seen in
   Lecture 5, but this time using 'recList' instead of explicit
   recursion. -}

insert :: Ord a => a -> [a] -> [a]
insert = undefined

{- 1 mark -}

{- 3.8 Define 'iterRight' from 'recList', without using explicit
   recursion. -}
iterRightFromRecList :: (a -> b -> b) -> b -> [a] -> b
iterRightFromRecList = undefined

{- 1 mark -}

{- For 3.8, it may be helpful to know that you can write lambda
   functions that take pairs as arguments using pattern matching
   notation. For example:

    \(a,b) -> a
-}

{----------------------------------------------------------------------}
{- 3.10 WILL APPEAR IN THE LAB TEST                                   -}
{-                                                                    -}
{- please leave this comment alone until further notice:              -}
{- we'll release an update to it on GitHub at the start of the test   -}
{- 3 marks -}
{----------------------------------------------------------------------}

{- Here is the Tree type again. Trees are built from 'Leaf's and
   'Node's, and each node has two children and a value of type
   'a'. Just as for lists, we can write higher-order functions that
   process trees. -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)

{- 3.11 The 'mapTree' function applies a given function to every value
   stored within the tree, returning the new tree. For example

     mapTree (+1) (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
  ==
     Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)

  Define this function by recursion over trees. -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

{- 2 marks -}

{- 3.12 'iterTree' is similar to 'iterLeft' and 'iterRight' for
   lists. 'iterTree l n t' uses 'n' as the value for every leaf in the
   tree and uses 'n' compute a value for every Node. Define this
   function. -}

iterTree :: b -> (b -> a -> b -> b) -> Tree a -> b
iterTree = undefined

{- 2 marks -}

{- 3.13 'recTree' is similar to 'recList', but operates on trees. Copy
   your definition of 'iterTree' and extend it to have the type given
   below. -}

recTree :: b -> ((Tree a,b) -> a -> (Tree a,b) -> b) -> Tree a -> b
recTree = undefined

{- 2 marks -}

{- 3.14 sumTree (from iterTree) -}
sumTree :: Tree Int -> Int
sumTree = undefined

{- 1 mark -}

{- 3.15 flatten from iterTree -}
flatten :: Tree a -> [a]
flatten = undefined

{- 1 mark -}

{----------------------------------------------------------------------}
{- 3.16 WILL APPEAR IN THE LAB TEST                                   -}
{-                                                                    -}
{- please leave this comment alone until further notice:              -}
{- we'll release an update to it on GitHub at the start of the test   -}
{- 1 mark -}
{----------------------------------------------------------------------}

{- 3.17 Write 'insertFromRecTree' (assuming that the tree is ordered),
   using 'recTree'. -}
insertFromRecTree :: Ord a => a -> Tree a -> Tree a
insertFromRecTree = undefined

{- 2 marks -}

{- 3.18 Define recTree from iterTree -}

type Extra a b = () -- REPLACE THIS

recTreeFromIterTree :: b -> ((Tree a,b) -> a -> (Tree a,b) -> b) -> Tree a -> b
recTreeFromIterTree n l t = snd (iterTree leaf node t)
  where
    -- leaf :: (Extra a b, b)
    leaf = undefined

    -- node :: (Extra a b, b) -> a -> (Extra a b, b) -> (Extra a b, b)
    node = undefined

{- 3 marks -}

{----------------------------------------------------------------------}
{- 3.19 WILL APPEAR IN THE LAB TEST                                   -}
{-                                                                    -}
{- please leave this comment alone until further notice:              -}
{- we'll release an update to it on GitHub at the start of the test   -}
{- 2 marks -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- MODELLING COMMUNICATING PROCESSES                                  -}
{----------------------------------------------------------------------}

{- This exercise generalises the communicating processes from Exercise 2
   to allow processes that send and recieve data of any type, not just
   bits. These processes are also a kind of tree, except that now the
   number of choices of what to do next after an input is represented
   by a function. -}

{- We'll do the setup, then it'll be your turn. -}

data CP x a
  = End a -- marks the end of a process, returning a value of type a
  | Input (x -> CP x a) -- (Input k) inputs a value v of type x, and
                        -- chooses a continuation process (k v) based
                        -- on that value.
  | Output x (CP x a) -- (Output v k) outputs a value v of type x and
                      -- continues as the process k.
  | Abort -- signals that the process has aborted. This can happen if
          -- it gets unexpected input.

{- Just in the 'Process' type in Exercise 2, the data in this type are
   descriptions of processes. We will see different ways of
   interpreting them below. -}

{- Let's have some example processes. First, the notGate example from
   Exercise 2, rewritten to be a member of the more general CP type: -}

notGate :: CP Bool ()
notGate = Input (\b -> Output (not b) (End ()))

{- See how this is the same as the notGate example in Exercise 2, only
   here instead of explicitly giving the two different options for the
   two possible inputs, we give a function that decides what to do
   instead. In this case, it outputs the 'not' of whatever the input
   is. -}

{- Let's have an example process: this process inputs any value, and
   then outputs that same value. -}

echo :: CP x ()
echo = Input (\v -> Output v (End ()))

{- We make processes 'go' in the same way as we did before. We interpret
   them, feeding the 'Input's from a list of inputs, and placing the
   'Output's into a list. There are two main differences with
   'process' from Ex 2: we need to return the extra value attached to
   'End', and we need to interpret the new 'Abort' instruction. -}

process :: CP x a -> [x] -> (a,[x])
process (End a)      inputs     = (a,[])
process (Input k)    (v:inputs) = process (k v) inputs
process (Input k)    []         = error "Not enough input"
process (Output v k) inputs     = (a,v:outputs)
  where (a,outputs) = process k inputs
process Abort        inputs     = error "ABORT"

{- For example,

   process echo ["Hello"] == ((),["Hello"])
-}

{- If we have a process that communicates using 'String's, then we can
   make it actually interact with the user using 'runIO'. This
   function translates process descriptions into I/O commands. This
   function uses Haskell's basic facilites for doing real I/O. We will
   come back to this later in the course. -}

runIO :: CP String a -> IO a
runIO (End a)      = return a
runIO (Input k)    = getLine >>= runIO . k
runIO (Output x k) = putStrLn x >> runIO k
runIO Abort        = error "ABORT"

{- Here's an example of using 'runIO'. The '>' is the haskell prompt.

   > runIO echo
   hello
   hello

   where the first 'hello' is typed by the user, and the second is
   printed by the computer. -}

{- Let's make some basic processes that we can use to build larger
   processes. Your job is to write these from their specifications. -}

{- 3.20 Define 'input'. FIXME: input spec -}
input :: CP x x
input = undefined

{- 1 mark -}

{- 3.21 Define 'output'. FIXME: output spec -}
output :: x -> CP x ()
output x = undefined

{- 1 mark -}

{- 3.22 Sequential composition of processes. -}

sequ :: CP x a -> (a -> CP x b) -> CP x b
sequ (End a)      f = f a
sequ (Input k)    f = Input undefined
sequ (Output x k) f = undefined
sequ Abort        f = undefined

{- 3 marks -}

{- 3.23 Define a process that does the same thing as 'echo' above, but
   using only 'input', 'output' and 'sequ'. -}

echoFromSequ :: CP x ()
echoFromSequ = undefined

{- 1 mark -}

{- 3.24 Adds the inputs, but doesn't output them. -}
addInputs :: CP Int Int
addInputs = undefined

{- 2 marks -}

{- Some useful operators defined using 'sequ' -}

{- 3.25 Define cpApply. -}

cpApply :: CP x (a -> b) -> CP x a -> CP x b
cpApply pf pa = undefined

{- 2 marks -}

{- 3.26 Now write addInputs again, but this time using 'input' and
 'cpApply'. -}

addInputs2 :: CP Int Int
addInputs2 = undefined

{- 1 mark -}

{----------------------------------------------------------------------}
{- 3.27 and 3.28 WILL APPEAR IN THE LAB TEST                          -}
{-                                                                    -}
{- please leave this comment alone until further notice:              -}
{- we'll release an update to it on GitHub at the start of the test   -}
{- 3 marks -}
{- 1 mark -}
{----------------------------------------------------------------------}

{- 3.29 Piping -}

pipe :: CP x a -> CP x b -> CP x b
pipe p1            (End b)       = undefined
pipe p1            (Output x p2) = undefined
pipe (End _)       (Input _)     = Abort
pipe (Output x p1) (Input p2)    = undefined
pipe (Input p1)    p2            = undefined
pipe Abort         _             = undefined
pipe _             Abort         = undefined

{- 5 marks -}

{- 3.30 Knock-knock jokes!

   Exhaustive research has discovered that users love it when their
   computers tell them jokes. -}

knockKnocker :: String -> String -> CP String ()
knockKnocker who clarification =
  undefined

{- 2 marks -}

{----------------------------------------------------------------------}
{- 3.31, 3.32, 3.33 and 3.34 WILL APPEAR IN THE LAB TEST              -}
{-                                                                    -}
{- please leave this comment alone until further notice:              -}
{- we'll release an update to it on GitHub at the start of the test   -}
{- 2 marks -}
{- 1 mark -}
{- 3 marks -}
{- 2 marks -}
{----------------------------------------------------------------------}
