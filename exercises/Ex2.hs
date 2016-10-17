module Ex2 where

{----------------------------------------------------------------------}
{- CS316 (2016/17) EXERCISE 2                                         -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 17 October.
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
{- FIRST-ORDER PROGRAMMING                                            -}
{----------------------------------------------------------------------}

{-
The *order* of a type is a crude measure of how "functiony" it is,
which in turn tells you how weird its values are likely to be.
It's not a Haskell program, because types aren't values, but we
write it using Haskell notation

order Bool      = 0
order Char      = 0
order Int       = 0
order [x]       = order x
order (s, t)    = max (order s) (order t)
order (s -> t)  = max (1 + order s) (order t)
  -- function types make types more functiony

In general, the order of a data type is the max of the orders
of the data stored in it.

Ordinary data are 0th order.
Programs which compute 0th order data from 0th order data are
1st order.
Programs which compute 0th or 1st order data from 1st order data
are 2nd order.
And so on.

All the types of the programs we ask you to write in this exercise
are at most 1st order.
-}


{- 2.1 Identify yourself. Write your name instead of Harry's between the
   quotation marks. Your file might get separated from your repository,
   so we'll need this info to give you your mark. -}

myName :: String
myName = "Harry Palmer"

{- 1 mark -}

{----------------------------------------------------------------------}
{- STRUCTURAL RECURSION ON TREES AND LISTS                            -}
{----------------------------------------------------------------------}

{- 2.2 Concatenation. The infix operator ++ concatenates two lists. Use
it to write a function in pattern matching style which concatenates a
list of lists. We have given you an unfinished definition which you
should refine into suitable cases and complete. -}

concLists :: [[x]] -> [x]
concLists xss = undefined    -- complete this definition

{- It may help to think concretely:
   (a) What should
     concLists [[1], [2,3], [4,5,6]]
   be?

   (b) What should
     concLists [[2,3], [4,5,6]]
   be?

   (c) How do you combine [1] with the answer to (b) to make the answer
   to (a)?
-}

{- 2 marks -}

{- 2.3 Mirror. Here are the trees from Exercise 1 again. -}

data {-xt-}Tree x                             -- {-xt-} gives naming convention
  = Leaf
  | Node  {-xlt-}(Tree x) x {-xrt-}(Tree x)   -- {-..-} names the pieces
  deriving Show  -- so you can see what you're doing!

{- Write a function to compute the mirror image of a tree, with left
   and right subtrees swapped at every level. For example

   mirror (Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
                4
                (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf)))
should be
           Node (Node (Node Leaf 7 Leaf) 6 (Node Leaf 5 Leaf))
                4
                (Node (Node Leaf 3 Leaf) 2 (Node Leaf 1 Leaf)))
-}

mirror :: Tree x -> Tree x
mirror t = undefined       -- you write this

{- Think concretely. Use the above example to figure out how mirror
   should work. -}

{- 3 marks -}

{- QUESTION: For a finite tree t, what should

   mirror (mirror t)

   be?

   ANSWER: write your answer here
-}

{- 1 mark -}

{- 2.4 Left spines -}

{- The "left spine" of a binary tree is the list of node labels starting
   at the root, and descending into each left subtree until the leftmost
   leaf is reached. Write a function to compute the left spine of a tree. -}

leftSpine :: Tree x -> [x]
leftSpine t = undefined     -- refine and complete

{- For example, you should have

   leftSpine (Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
                   4
                   (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf)))
   = [4,2,1]
-}

{- 2 marks -}

{----------------------------------------------------------------------}
{- 2.5 Right spines (TEST)                                            -}

{- The "right spine" is defined similarly, except that we start at the root
   and descend into each right subtree. Write a function to compute the right
   spine of a tree. -}

rightSpine :: Tree x -> [x]
rightSpine t = undefined -- you should define this

{- 2 marks -}

{- QUESTION: Can you think of an equational law which relates leftSpine,
   rightSpine and mirror, for any finite tree t?

   ANSWER: write your answer here
-}

{- 1 marks -}

{----------------------------------------------------------------------}

{- 2.6 FindInTree. Your mission is to test if a given value is somewhere in a
     tree, under various assumptions. -}

{- Firstly, assuming you can test values only for equality, write a function
   to test if a given value is in a given tree. You may want to use the "or"
   operator which is written || and applies to a pair of Bool values.
   The "Eq x =>" bit in the type below means "assuming you can test elements
   of x for equality with the == operator" -}

eqFindInTree :: Eq x => x -> Tree x -> Bool
eqFindInTree x t = undefined      -- refine and complete

{- 4 marks -}

{----------------------------------------------------------------------}
{- 2.7 finding things efficiently in trees, using an ordering (TEST)  -}

{- Secondly, assuming you can test values for order and equality
   (with <, >, ==), as given by the "Ord x =>" constraint in the type, an
   that the input trees are binary search trees, write an *efficient* function
   which tests if an element is in a tree.
-}

ordFindInTree :: Ord x => x -> Tree x -> Bool
ordFindInTree x t = undefined -- write this function

{- 5 marks -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- MODELLING COMMUNICATING PROCESSES                                  -}
{----------------------------------------------------------------------}

{- This exercise is about modelling processes which input and output
   bits. Processes are things. They're a kind of tree, representing a
   decision process, given by the following datatype. -}

{- We'll do the setup, then it'll be your turn. -}

data {-p-}Process  -- {-..-} is again for shplit purposes
  = End    -- marks the end of the process, so no more input or output
  | Output {-b-}Bool {-p-}Process 
           -- (Output b p) outputs bit b, then continues as p
  | IfInput {-tp-}Process {-fp-}Process
           -- (IfInput tp fp) inputs a bit, continuing as tp if it's
           -- True, fp if False
  deriving Show

{- Don't expect the data in this type to *do* anything! Rather, they
   *represent* processes. We'll see how to interpret them shortly. -}

{- Let's have an example process: this process should output False if its
   input is True and True if its input is False. -}

notGate :: Process
notGate = IfInput (Output False End) (Output True End)

{- See? If the input is True, we take the left path and find
   (Output False End), otherwise we go right and find (Output True End).
   Either way, we make one output and then stop. -}

{- How can we make processes go? We need to interpret them. Here's how.
   The "process" function takes a Process to interpret, and a list of input
   bits in [Bool], then produces the list of output bits. -}

process :: Process -> [Bool] -> [Bool]
process End              bs            = []
  -- when we're at the end, there is no more output
process (Output b p)     bs        = b : process p bs
  -- the output from (Output b p) had better begin with b, and the rest
  -- is whatever comes out from running p on the input
process (IfInput tp fp)  (b : bs)  = process (if b then tp else fp) bs
  -- when the process wants input, the result depends on the first bit
  -- in the input list: if that's True, then we continue with the tp
  -- branch; if it's false, we continue with the fp branch. In both
  -- cases, we feed the rest of the input bs to the continuing process
process (IfInput tp fp)  []        = []
  -- in the unfortunate case where the process wants input but the input
  -- list is empty, we're a bit stuck; let's stop and return no output

{- Let's try it out. Here are some test examples. Try loading this file in
   ghci, then evaluating testNotT and testNotF at the prompt. Remember, you
   can evaluate an expression (e.g. testNotT) just by typing it in and
   pressing enter. Do you get what you expect? -}

testNotT :: [Bool]
testNotT = process notGate [True]

testNotF :: [Bool]
testNotF = process notGate [False]


{- 2.8 The Wire. Make a process, rather like the notGate, which reads one
   bit of input and sends that one bit unaltered to the output. -}

wire :: Process
wire = undefined     -- you define it

{- 1 mark -}

{- use ghci to check that
     process wire [True]   =  [True]
     process wire [False]  =  [False]
-}

{- Let's make some gates with two inputs. Here's 'or'. -}

orGate :: Process
orGate = IfInput (IfInput (Output True End) (Output True End))
                 (IfInput (Output True End) (Output False End))

{- use ghci to check that
     process orGate [True,True]    = [True]
     process orGate [True,False]   = [True]
     process orGate [False,True]   = [True]
     process orGate [False,False]  = [False]
-}


{- 2.9 Xor, And. Make processes in the style of orGate which act like
   an xor-gate and an and-gate, respectively. -}

xorGate :: Process
xorGate = undefined  -- you define it

andGate :: Process
andGate = undefined  -- you define it

{- 3 marks the pair -}

{- you can use ghci to check these also work as they should -}


{- 2.10 Ignoring input. You can see there's quite a bit of repetition
   in orGate and andGate. For example, if the first input to orGate
   is True, then the output must be True, irrespective of the second
   input. But we do have to consume the input, even if it makes no
   difference. Write a function... -}

always :: Process -> Process
always p = undefined     -- you define it

{- ... which takes a process p and produces the process which reads
   one input, then ignores it and acts like p in *both* cases. -}

{- 1 mark -}

{- You should find that you can simplify your gates. If you've got
   "wire" and "always" right, you should find this new definition
   of orGate works just the same as the old one. -}

orGate' :: Process
orGate' = IfInput (always (Output True End)) wire

{- See? If the first input is True, then the output is always True,
   regardless of the second. If the first input is False, then the
   process behaves like a wire, transmitting the second input as is. -}


{- 2.11 Tidy up your xorGate and andGate in the same way. Make use of
   wire, notGate, always, etc. Small is beautiful. -}

xorGate' :: Process
xorGate' = undefined     -- you define it

andGate' :: Process
andGate' = undefined     -- you define it

{- 2 marks -}

{----------------------------------------------------------------------}
{- 2.12 Further gates (TEST)                                          -}

{- Define a process which reads two inputs and implements a nand-gate;
that is, it should return True exactly when both of its inputs are not
True. Use wire, notGate, always, etc. if you can. -}

nandGate :: Process
nandGate = undefined -- you write this

{- use ghci to check that
     process nandGate [False,False] = [True]
     process nandGate [False,True]  = [True]
     process nandGate [True,False]  = [True]
     process nandGate [True,True]   = [False]
-}

{- Define a process which reads two bits of input and returns True if
they are the same, False otherwise. Again, make use of wire, notGate,
always, etc. when possible. -}

equalGate :: Process
equalGate = undefined -- you write this

{- use ghci to check that
     process equalGate [True,True]   = [True]
     process equalGate [False,False] = [True]
     process equalGate [False,True]  = [False]
     process equalGate [True,False]  = [False]
-}

{- 2 marks -}
{----------------------------------------------------------------------}

{- 2.13 Copy1. Write a process which reads *one* bit of input, then outputs
   that bit *twice*. -}

copy1 :: Process
copy1 = undefined     -- you define it

{- 1 mark -}


{- 2.14 Swap2. Write a process which reads *two* bits of input, then outputs
   them in reverse order. -}

swap2 :: Process
swap2 = undefined     -- you define it

{- 2 marks -}

{- Remember to use the "process" function to check your answer in ghci! -}

{- Now let's build some kit to put small processes together to make larger
   processes, the way you build circuits up. -}


{- 2.15 Sequencing processes. Write a function which combines two processes
   in sequence, so that the second begins once the first has ended.
   That is, you should 'graft' the second process in place of all the End
   markers in the first process.  -}

sequ :: Process -> Process -> Process
sequ p p' = undefined     -- you define it

{- To check that you've got it right, make sure that

   process (sequ notGate notGate) [True,True]   = [False,False]
   process (sequ notGate notGate) [True,False]  = [False,True]
   process (sequ notGate notGate) [False,True]  = [True,False]
   process (sequ notGate notGate) [False,False] = [True,True]

That is, sequencing two notGate components gives you a process which negates
two inputs. -}

{- 3 marks -}


{- 2.16 Pipelining processes. Write a function which combines two processes
   so that the output from the first is used as the input for the second.
   That is, the combined process should keep the inputs from the first
   process and the outputs from the second process, but hide the communication
   in the middle. Give priority to the second process, so the first runs only
   when the second is demanding input. We've done some of it for you, but
   you may still need to refine the pattern match further. -}

pipe :: Process -> Process -> Process
pipe p1 End = End
pipe p1 (Output b p2) = Output b undefined  -- what happens next?
pipe End (IfInput t f) = End
  -- the second process is hungry, but it starves to death!
pipe (Output b p) (IfInput t f) = undefined  -- what's this?
  -- communication: the first process is ready to output, the second
  -- wants to input, so the output from the first should determine
  -- what happens next, somehow
pipe (IfInput t1 f1) (IfInput t2 f2) =
  IfInput undefined undefined  -- what happens in each case?
  -- the second process is hungry, and so is the first, so ask 'the world'
  -- for some input

{- 5 marks -}

{- Here's an example. If we pipe the output from an orGate into a notGate,
   we should get a norGate. -}

norGate :: Process
norGate = pipe orGate notGate

{- check that

    process norGate [True,True]   = [False]
    process norGate [True,False]  = [False]
    process norGate [False,True]  = [False]
    process norGate [False,False] = [True]

   and have a look at the value of norGate to see what you get
-}

{- A typical circuit will pipe data through various stages, with each
   stage being given by sequ-ing components together. -}


{- 2.17 Copy2. Use pipe and sequ to build a process which reads *two*
   bits, then outputs *four* in the order first-second-first-second.
   That is, the process should output two copies of a 2-bit input.
   You should find copy1, wire and swap2 to be handy for this purpose.
-}

copy2 :: Process
copy2 = undefined     -- you define it

{- 3 marks -}

{- check that

    process copy2 [True,True]   = [True,True,True,True]
    process copy2 [True,False]  = [True,False,True,False]
    process copy2 [False,True]  = [False,True,False,True]
    process copy2 [False,False] = [False,False,False,False]

   and have a look at the value of copy2 to see what you get
-}

{- 2.18 Half Adder. Using copy2, and possibly some other gates you've
   built already, construct a half adder process, adding two 1-bit
   inputs to make a 2-bit output. -}

hadd :: Process
hadd = undefined     -- you define it

{- remember that a half-adder implements the binary addition

    0 + 0 = 00
    0 + 1 = 01
    1 + 0 = 01
    1 + 1 = 10

   that is you should check that hadd satisfies

    process hadd [False,False] = [False,False]
    process hadd [False,True]  = [False,True]
    process hadd [True,False]  = [False,True]
    process hadd [True,True]   = [True,False]

   and have a look at the value of hadd to see what you get
-}

{- 3 marks -}


{- 2.19 Full Adder. Using hadd twice, a gate and some other wiring
   components, construct a full adder, adding three 1-bit inputs
   to get a 2-bit output in the range 0..3 -}

fadd :: Process
fadd = undefined     -- you define it

{- check that

    process fadd [True,True,True]    = [True,True]
    process fadd [True,True,False]   = [True,False]
    process fadd [True,False,True]   = [True,False]
    process fadd [True,False,False]  = [False,True]
    process fadd [False,True,True]   = [True,False]
    process fadd [False,True,False]  = [False,True]
    process fadd [False,False,True]  = [False,True]
    process fadd [False,False,False] = [False,False]

   and have a look at the value of fadd to see what you get
-}

{- 3 marks -}

{----------------------------------------------------------------------}
{- 2.20 process with left-over input, and error handling (TEST)       -}


{- The current process function above suffers from two problems:

   1. It can starve, that is expect more input when there is none (not
      enough input), and
   2. it can finish without consuming all its input (too much input)

   Fix these problems by defining a new function

    process' :: Process -> [Bool] -> Maybe ([Bool], [Bool])

   which returns Nothing if it is starving, and Just (os, ls)
   otherwise, where os is the list of output bits, and ls the list of
   unconsumed inputs.

   Examples:

    process' (IfInput wire wire) []     = Nothing
    process' End [True, False]          = Just ([], [True, False])
    process' (IfInput wire wire) [True] = Just ([True], [])

-}

process' :: Process -> [Bool] -> Maybe ([Bool], [Bool])
process' p bs = undefined -- you write this

{- 5 marks -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- 2.21 Expecting input (TEST)                                        -}

{- Write a function

     expected :: [Bool] -> Process

   which, given a list of expected inputs of length n, reads n inputs
   and outputs True if and only if the input matched the expected one.
   The process should not output anything before it has read exactly n
   inputs.

   Hint: You might find it helpful to write an auxiliary function which
   passes along a parameter to keep track of if the input so far matches
   the expected one or not.

   Check that e.g.

   process (expected [True, False, True]) [True, False, True]  = [True]
   process (expected [True, False, True]) [True, False, False] = [False]

-}

expected :: [Bool] -> Process
expected is = undefined -- you write this

{- 5 marks -}
{----------------------------------------------------------------------}
