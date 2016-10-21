module Lec10 where

{- LECTURE 10: BUILDING PURE EVALUATORS 

   We ended the previous lecture looking at a data type of arithmetic
   expressions: -}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

{- 'Expr's are binary trees, with 'Int's at the leaves and every node
   labeled with 'Add'. Here is an example 'Expr': -}

myExpr :: Expr
myExpr = (Number 2 `Add` Number 5) `Add` (Number 34 `Add` Number 12)

{- representing the expression:

      (2 + 5) + (34 + 12)

   Note that the bracketing is important. Even though we know that it
   does not matter what order we bracket actual addition, we are not
   doing actual addition yet. 'Expr' is a type of _abstract_ syntax
   trees for expressions.

   In Lecture 9, we also derived the iterator function for computing
   functions recursively over 'Expr's: -}

iterExpr :: (Int -> t) -> (t -> t -> t) -> Expr -> t
iterExpr number add (Number i) = number i
iterExpr number add (Add d e) =
    add (iterExpr number add d) (iterExpr number add e)

{- We will also make use of the iterator for lists: -}

iterList :: b -> (a -> b -> b) -> [a] -> b
iterList nil cons []     = nil
iterList nil cons (x:xs) = cons x (iterList nil cons xs)

----------------------------------------------------------------------

{- Values of type 'Expr' represent abstract syntax trees of arithmetic
   expressions. To interpret an 'Expr' using actual arithmetic, we
   have to describe what to do for each constructor in 'Expr'. This is
   exactly what 'iterExpr' is designed to do.

   The 'evaluate' function defined here uses 'iterExpr' to replace
   each constructor in an 'Expr' with its "meaning". We make the
   decision that the meaning of 'Number n' is just 'n', represented
   using the identity function 'id'. The meaning of 'Add' is the
   actual '+' function -- matching our intuition about how to
   interpret addition. -}

evaluate :: Expr -> Int
evaluate = iterExpr
             id  -- "Meaning" of Number
             (+) -- "Meaning" of Add

{- 'evaluate' describes the standard meaning for each 'Expr'. But this
   is not the only "meaning" we can assign to 'Expr's. A useful
   technique is to evaluate 'Expr's using a non-standard
   interpretation that models some aspect of the standard
   meaning. This allows us to compute a normalised or optimised
   version of an 'Expr' without actually evaluating it.

   We will model the fact that addition is associative, meaning that
   it doesn't matter which bracketing is used in the representation of
   the term. For example, the syntax my 'myExpr' above is:

      (2 + 5) + (34 + 12)

   But this evaluates to the same answer as:

      2 + (5 + (34 + (12 + 0)))

   where we have "normalised" the expression by always nesting
   brackets to the right, and ending with a 0.

   Is it possible to convert programs from the first form to the
   second in a nice way? One way to do this is to 'interpret' the
   original expression in a non-standard semantics that captures the
   fact that addition is associative, and nothing more, and then to
   convert that non-standard semantics back to an 'Expr'.

   To capture the fact that addition is associative, we observe that
   the fact that bracketing does not matter means that it always
   suffices to add up numbers from left to right sequentially. We
   represent this sequencing behaviour by intepreting 'Expr's as lists
   of their numbers. We convert every Expr into a list of numbers to
   sum up. We do this with another use of 'iterExpr': -}

evalToList :: Expr -> [Int]
evalToList = iterExpr (\ x -> [x]) (++)

{- So we have interpreted (Number n) as [n], meaning that "Number n" is
   the sum of just one number, "n"; and we have interpreted (Add e1
   e2) as the list of numbers to add up for e1, appended with the list
   of numbers to add up for e2.

   Trying this out, we get:

     λ> evalToList myExpr
     [2,5,34,12]

   Which we read as "to evaluate myExpr, we will have to sum up the
   numbers [2,5,34,12]".

   To turn lists of numbers back into 'Expr's, we use 'iterList'. The
   empty list '[]' represents the sum of no numbers, which is
   represented by 'Number 0'. A cons 'n:ns' represents adding 'n' to
   the result of summing 'ns', so we turn it into an 'Add' expression. -}

normalise :: Expr -> Expr
normalise = (iterList (Number 0) (\ n e -> Add (Number n) e)) . evalToList

{- Now,

     λ> normalise myExpr
     Add (Number 2) (Add (Number 5) (Add (Number 34) (Add (Number 12) (Number 0))))

   which is the representation of the normal form:

      2 + (5 + (34 + (12 + 0)))
-}

{- Part 2. Expressions with exceptions

   The 'Expr' type above described "pure" arithmetic expressions that
   always evaluate to a value. Often programming languages have
   facilities that allow for "non-pure" side effects to happen during
   the program.

   The following data type 'Expr2' extends 'Expr' with two new
   constructors: 'Throw2' and 'Catch2'. The intention is that 'Throw2'
   represents the action of throwing an exception, and 'Catch2'
   represents a try-catch style exception handler. The first argument
   to 'Catch2' is the expression to try, and the second argument is
   the exception handler.
-}

data Expr2
  = Number2 Int
  | Add2    Expr2 Expr2
  | Throw2
  | Catch2  Expr2 Expr2
  deriving Show

{- An example program using Throw2 and Catch2 is this one: -}

myProblemProgram :: Expr2
myProblemProgram =
  (Add2 (Number2 12) Throw2) `Catch2` (Number2 123)

{- This program attempts to add two numbers, but one of those numbers is
   faulty, so it throws an exception which is caught by an exception
   handler which handles it with a handler that always returns '123'.

   To define an evaluator for 'Expr2's, we might start by trying to
   write a function of type:

     evalExpr2 :: Expr2 -> Int

   After all, evaluation of Expr2 should still result in integers
   being returned. However, this type does not model the fact that
   evaluation of an 'Expr2' may fail with an exception. Therefore, we
   need to adjust the type of the return value of 'evalExpr2' to
   account for the possibility of throwing an exception. We do this by
   stating that evaluation returns 'Maybe Int' -- so it can either be
   'Nothing' (when an exception is thrown), or 'Just n' (when
   evaluation returns normally). -}

evalExpr2 :: Expr2 -> Maybe Int
{- We'll write this function using pattern matching instead of using
   some iterExpr2, just because it is slightly clearer to see what is
   going on.

   For the 'Number2' case, we always return 'Just n', because there is
   no way to throw an exception while evaluating a number. -}
evalExpr2 (Number2 n) = Just n
{- For the 'Add2' case, we have to evaluate 'e1' and 'e2', but we also
   have to deal with the possibility that evaluating either of them
   may cause an exception to be thrown, which we should propagate to
   the final answer. We do this by using a cascade of 'case's: -}
evalExpr2 (Add2 e1 e2) =
  case evalExpr2 e1 of
    Nothing -> Nothing
    Just n1 ->
      case evalExpr2 e2 of
        Nothing -> Nothing
        Just n2 -> Just (n1 + n2)
{- To interpret 'Throw2', we use 'Nothing' to represent the case when an
   exception is thrown. -}
evalExpr2 Throw2 = Nothing
{- Finally, for the 'Catch2' case, we evaluate the first expression. If
   it returns a value, we just return that value. If it fails with
   'Nothing', then we evaluate the exception handler and use its
   result as the result of evaluating the whole 'Catch2' expression. -}
evalExpr2 (Catch2 e1 e2) =
  case evalExpr2 e1 of
    Nothing -> evalExpr2 e2
    Just n -> Just n

{- Now evaluating our test program shows the exception throwing and
   handling working:

     λ> evalExpr2 myProblemProgram
     Just 123
-}

{- Part 3. Expressions with printing

   Another possible side effect we might have during execution of a
   program is the printing of logging messages. We extend the 'Expr'
   type to include the possibility of printing by adding a constructor
   'Print message e'. The intended meaning is that this prints the
   message 'message' and then executes 'e'. -}

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  | Print   String Expr3
  deriving Show

{- An example program using this new feature is the following, which
   intersperses some arithmetic with instructions to print out some
   messages: -}

printProg :: Expr3
printProg = Print "Hello" (Add3 (Number3 5) (Print " World" (Number3 42)))

{- To evaluate expressions with printing, we keep a log of all the
   messages that are printed, in order. We represent this log using a
   list. Therefore, the result type of our evaluator is a pair of the
   list of strings printed, and the resulting integer: -}
evalExpr3 :: Expr3 -> ([String], Int)
{- Evaluating a number 'n' results in no messages being printed, and the
   number 'n' as the final answer. -}
evalExpr3 (Number3 n) = ([] , n)
{- Evaluating 'Add3 e1 e2' means we must evaluate 'e1', getting the
   messages printed during that evaluation and its integer value, then
   we evaluate 'e2' getting the second sequence of messages and its
   integer value. Finally, we combine the lists of messages and the
   integers. -}
evalExpr3 (Add3 e1 e2) =
  let (ds1, n1) = evalExpr3 e1
      (ds2, n2) = evalExpr3 e2
  in
  (ds1 ++ ds2 , n1 + n2)
{- Evaluating 'Print' is where we actually add messages to the log -- if
   we didn't have Print then the only lists of messages you can build
   from the empty list and append are the empty list! Printing
   evaluates its second argument to get its result and list of
   messages, and then prepends the new message to the log: -}
evalExpr3 (Print message e) = case evalExpr3 e of
  (ds, n) -> (message : ds, n)

{- Evaluating our test program gives us the messages and result we expect:

       λ> evalExpr3 printProg
       (["Hello"," World"],47)
-}

{- Part 4. Expressions with choice

   A final side effect we will look at here is non-determinism. To the
   original 'Expr' data type, we add 'Choice' which takes two
   arguments and somehow makes a choice between them. There are
   several different reasonable interpretations of Choice, and we will
   look at two of them. -}

data Expr4
  = Number4 Int
  | Add4    Expr4 Expr4
  | Choice  Expr4 Expr4
  deriving Show

{- Here is an example program that uses 'Choice'. It adds two numbers
   together, but one of those numbers is not fully determined: it
   could either be '0' or '1'. -}

myDitheringProgram :: Expr4
myDitheringProgram =
  (Choice (Number4 0) (Number4 1)) `Add4` (Number4 2)

{- A first attempt at writing an evaluator for expressions with choice
   might have type:

       evalExpr4 :: Expr4 -> Int

   However, we get stuck when trying to evaluate (Choice e1 e2):

       evalExpr4 (Choice e1 e2) = ???

   We must return a single integer, but we have a choice of two
   expressions to evaluate to get integers! There are several ways out
   of this situation:

     1. We could evaluate both 'e1' and 'e2' and combine their answers
        somehow -- taking their sum, or maximum, or something. This
        seems intuitively to not be faithful to the notion of
        'Choice'.

     2. We could always take 'e1' or always take 'e2'. So we build a
        'biased' interpreter that always makes choices for us. This is
        reasonable, but we could be more general.

     3. We could assume that we are given a supply of booleans that
        tells us how to resolve each choice in turn.

     4. We could return all possible choices, perhaps as a list.

   Options 3 and 4 seem reasonable and interesting, so let's implement
   them.

   We implement Option 3 by writing a function of the following type: -}
evalExpr4opt3 :: Expr4 -> [Bool] -> (Int, [Bool])
{- After we take an Expr4, we take a list of 'Bool's that will tell us
   how to resolve each choice in turn. We then return the integer
   values resulting from evaluating the expression with those choices,
   and the left-over list of choices.

   Evaluating a "pure" number results in just that number, and the
   list of choices is passed through unaffected: -}
evalExpr4opt3 (Number4 n) choices =
  (n, choices)
{- Evaluating an addition means that we have to evaluate both
   sub-expressions, but we must be careful to "thread through" the
   list of choices: we evaluate 'e1' with the initial list, getting
   'choices1', which we use to evaluate 'e2', to get 'choices2', which
   we return. -}
evalExpr4opt3 (Add4 e1 e2) choices =
  let (n1, choices1) = evalExpr4opt3 e1 choices
      (n2, choices2) = evalExpr4opt3 e2 choices1
  in
    (n1 + n2, choices2)
{- Finally, evaluating 'Choice e1 e2' uses one of the choices from the
   list. For simplicity, we assume that we are given enough
   pre-determined choices to evaluate all the 'Choice's in the
   expression, so we don't handle the case with the empty list. -}
evalExpr4opt3 (Choice e1 e2) (c:choices) = 
  evalExpr4opt3 (if c then e1 else e2) choices

{- Evaluating 'myDitheringProgram' with a list of predetermined choices
   yields a single value, and the left-over choices:

       λ> evalExpr4opt3 myDitheringProgram [True, True]
       (2,[True])
       λ> evalExpr4opt3 myDitheringProgram [False, True]
       (3,[True])
-}

{- Option 4 that we listed above took a different approach to evaluating
   programs with 'Choice'. Instead of evaluating to a single anwer, we
   will evaluate to all the possible answers. We represent all the
   possible answers by a list. -}

evalExpr4opt4 :: Expr4 -> [Int]
{- Evaluating a single number has only one possibility, so we return it
   in a singleton list: -}
evalExpr4opt4 (Number4 n) = [n]
{- To evaluate an 'Add', we collect all the possibilities for evaluating
   the two sub-expressions, and then compute all the possible ways of
   adding them together, using a list comprehension. -}
evalExpr4opt4 (Add4 e1 e2) =
  let possibilities1 = evalExpr4opt4 e1
      possibilities2 = evalExpr4opt4 e2
  in
    [ n1 + n2 | n1 <- possibilities1, n2 <- possibilities2 ]
{- Evaluation of 'Choice' also collects all the possibilities for
   evaluating its sub-expressions, but then combines them using '++',
   so that we collect all the possible outcomes. -}
evalExpr4opt4 (Choice e1 e2) =
  let possibilities1 = evalExpr4opt4 e1
      possibilities2 = evalExpr4opt4 e2
  in
    possibilities1 ++ possibilities2

{- Evaluating 'myDitheringProgram' with 'evalExpr4opt4' now gives us all
   the possible results:

       λ> evalExpr4opt4 myDitheringProgram
       [2,3]
-}


{- EXERCISE: The following Expr5 type also adds the possibilty of
   'Failure' as well as choice. Write extended version of 'evalExpr4'
   and 'evalExpr4opt4' for this new type that also interpret
   Failure. You will have to change the type of 'evalExpr4'. -}

data Expr5
  = Number5 Int
  | Add5    Expr5 Expr5
  | Choice5 Expr5 Expr5
  | Failure5
  deriving Show


{- We have now seen four different toy languages, with five different
   evaluation functions that handled side effects. Even though all of
   these evaluators had different ways of handling side effects
   (exceptions, printing, and choice), we still had to write the basic
   code to plumb through the interpretation of 'Number' and 'Add' each
   time. In the next lecture, we will see how to tidy and abstract all
   this plumbing. -}
