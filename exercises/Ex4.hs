module Ex4 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List

{----------------------------------------------------------------------}
{- CS316 (2016/17) EXERCISE 4                                         -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 28th November.
-- There will be a test on this exercise in the lab on that date.
--
-- Your combined score from the submission and the test will be worth
-- 35% of the overall marks for the class (so one mark, below is worth
-- half a percent).
--
-- The test will consists of further requirements issued as updates to
-- this file, and you will need to make changes in response to the new
-- requirements, then commit a new version of the file by the end of
-- the lab session.

{----------------------------------------------------------------------}
{- GHOUL : Global Higher-order Untyped Language                       -}
{----------------------------------------------------------------------}

{- 4.1 Identify yourself. Encode your name instead of Harry's between the
   quotation marks. Your file might get separated from your repository,
   so we'll need this info to give you your mark. -}

myName :: String
myName = map pred "Ibssz!Qbmnfs"

{- 1 MARK -}




{- INTRODUCTION TO GHOUL

   This exercise is about building an interpreters for a functional
   language called "GHOUL". It will bring together all of the concepts
   you have learned during this course.

   Here is an example GHOUL program (the backslashes '\' at the start
   and end of the lines are how Haskell allows muolti-line strings): -}

plusProg :: String
plusProg =
  " plus(Z,y) = y;\
  \ plus(S(x),y) = S(plus(x,y));\
  \ main() = plus(S(S(Z)),S(S(Z)));"

{- Execution of GHOUL programs works by starting from the main()
   function and then matching each function application against the
   patterns defined for that function to get an expression to replace
   that application with. This continues until there are no more
   expression to replace. Data is built from constructors (identifiers
   that start with captial letters) applied to other constructors.

   For our example program, we have:

         main()
      => plus(S(S(Z)),S(S(Z)))
      => S(plus(S(Z),S(S(Z))))
      => S(S(plus(Z,S(S(Z)))))
      => S(S(S(S(Z))))

   So "two plus two is four".

   GHOUL is similar to Haskell, except that there are no types and
   there are no lambda expressions (\x -> ...).

   In this exercise, you will build a GHOUL interpreter, and extend it
   with additional features.

   In general, all interpreters perform the following steps:

     1. Parse the input -- take a representation of a program as a
        list of characters and turn it into abstract syntax. You will
        do this using parser combinators.

     2. Post-process and check the input, often called "Elaboration"
        and "Type Checking". In Haskell this is a complex stage
        involving type checking and inference. For GHOUL, you will
        need to perform some well-formedness checks on the program and
        prepare it to be ready for execution.

     3. Execute the program. In Lecture 11, we showed you evaluators
        for several very simple languages. GHOUL has two features that
        make evaluation more complex:

          a. Named function definitions.
          b. Pattern matching.

   These steps are summed up by the function that parses, elaborates
   and executes GHOUL programs: -}

runGHOUL :: String -> Maybe Val
runGHOUL text = do
  eqns <- runParser equations text
  prog <- elaborate eqns
  guard (scopeCheck prog)
  evalProgram prog

{- 'runGHOUL' accepts a String containing a GHOUL program, parses it,
   elaborates it, checks it, and executes it. If any of these steps
   fail, 'Nothing' is returned. Otherwise, the result of evaluating
   the program is returned.

   Of course, 'runGHOUL' doesn't work yet -- you will need to fill in
   the details below.

   This exercise is structured so that you can implement a basic GHOUL
   interpreter first, and then go back to extend it with additional
   features for extra marks. As with the previous exercises, roughly a
   third of the marks will only be available during the class test on
   Monday 28th November. -}

{----------------------------------------------------------------------}
{- Part 0 : ABSTRACT SYNTAX                                           -}
{----------------------------------------------------------------------}

{- Before we can write an interpreter for GHOUL programs, we need to
   describe what the syntax of GHOUL programs is. Generalising from
   the example above, a GHOUL program is:

     - a list of named function definitions; where
     - a definition is a list of equations; where
     - an equation is a list of patterns and an expression; where
     - a pattern is a variable name or a constructor name
       and list of patterns; and
     - an expression is a variable use, or an application of a named
       function to expressions, or an application of a constructor
       name to expressions.

   Also, there must be a function definition named 'main' that takes
   no arguments.

   Following this description, we represent GHOUL programs as values
   of the type 'Program'. This type doesn't yet capture the constraint
   that there is a definition named 'main' -- you will write some code
   to check this below. -}

type Program = [(String,Definition)]

{- Definitions are represented by the following data type. For
   convenience during execution of GHOUL programs we have stored the
   number of arguments that this definition expects as a separate
   field. The second field is a list of equations, each consisting of
   a list of patterns and an expression (defined below). -}

data Definition
  = MkDefinition { defArity   :: Int
                 , defClauses :: [([Pat], Exp)]
                 }
  deriving (Show, Eq)
             
{- To be well defined, every definition

       MkDefinition arity equations

   must satisfy the property that:

       all (\(pats, _) -> length pats == arity) equations

   That is: every list of patterns is the same length, and that length
   is 'arity'.

   We have named the fields in this data type so that they can be more
   easily accessed. This means that there are functions:

       defArity :: Definition -> Int
   and
       defClauses :: Definition -> [([Pat], Exp)] -}

{- A pattern is either a variable (PV), or a constructor name and a list
   of patterns (PC). This is similar to patterns in Haskell, except
   that GHOUL does not have a "catch all" pattern '_' -}

data Pat
  = PV String
  | PC String [Pat]
  deriving (Show, Eq)

{- An expression is either a variable (EV), an application of a named
   function (EA) or a an application of a constructor (EC). -}

data Exp
  = EV String
  | EA String [Exp]
  | EC String [Exp]
  deriving (Show, Eq)

{- Here is an example 'Program', representing the example program we saw
   above. It is worth spending time to understand the correspondence
   between the Haskell value below and the concrete syntax above. -}

plusProgram :: Program
plusProgram =
  [ ("plus",
     MkDefinition
      { defArity = 2
      , defClauses =
          [ ([PC "Z" [],       PV "y"], EV "y")
          , ([PC "S" [PV "x"], PV "y"], EC "S" [EA "plus" [EV"x", EV"y"]])
          ]
      })
  , ("main",
     MkDefinition
      { defArity = 0
      , defClauses =
          [ ([], EA "plus" [EC "S" [EC "S" [EC "Z" []]],
                            EC "S" [EC "S" [EC "Z" []]]])]
      })
  ]

{----------------------------------------------------------------------}
{- 4.2 TEST: Write a GHOUL program to concatenate lists.

   Write a GHOUL program to concatenate (append) two lists. You can
   use the example of the 'append' program written in Haskell given in
   Lecture 3 as a guide. -}

{- 3 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 1 : PARSING                                                   -}
{----------------------------------------------------------------------}

{- Writing GHOUL programs as values of type 'Program' is all very well,
   but not very friendly. Instead, we will build a parser and
   elaborator that will take a String that represents a GHOUL program
   and turn it into a list of equations. A list of equations is not
   yet a program, so Part 2 will build an elaborator to convert lists
   of equations into proper 'Program's.

   You will build your parser as a 'list of things and strings' parser
   of the following type, as introduced in Lecture 13: -}

newtype Parser a = MkParser (String -> [(a, String)])

{- Parsers are applied to 'String's by using the 'runParser' function: -}

runParser :: Parser a -> String -> Maybe a
runParser (MkParser p) input =
  case [ a | (a,"") <- p input ] of
    [a] -> Just a
    _   -> Nothing

{- 'runParser' only accepts if your parser accepts the whole input (so
   the leftover string is "") and if there is a unique result. When
   writing your parsers, you may find it helpful to see all of the
   results that your parser returns. Use the 'debugParser' function to
   do this: -}

debugParser :: Parser a -> String -> [(a,String)]
debugParser (MkParser p) input = p input

{- The rest of the parser combinator functions are at the end of this
   file. The main combinators that you will want to use to build your
   parsers are:

     - The Functor, Applicative, Monad, and Alternative interfaces
     - 'isChar' to parse given characters
     - 'string' is parse given strings
     - 'identifier' to parse identifiers: sequences of letters and numbers
       that must start with a letter.
     - 'spaces' to parse zero or more white space characters.
     - 'sepBy' to parse lists of things separated by something. -}

{- To begin the GHOUL parser, you will construct two parsers that
   recognise variable names and constructor names. We will use these
   later on as part of our complete pattern and expression parsers. -}


{- 4.3 Write a 'Parser' for 'variable names'.

   Follow the Haskell convention that a variable name is an identifier
   that starts with a lower case letter. Use the library function
   'isLower' to identify lower case letters. -}

varname :: Parser String
varname = undefined

{- Here are some tests that your 'varname' parser should pass:

     runParser varname "plus"  == Just "plus"
     runParser varname "x"     == Just "x"
     runParser varname "Plus"  == Nothing
     runParser varname ""      == Nothing
     runParser varname "plu s" == Nothing
     runParser varname "123"   == Nothing -}

{- 1 MARK -}


{- 4.4 Write a 'Parser' for 'constructor names'.

   Follow the convention that a constructor name is an identifier that
   starts with an upper case letter. Use the library function
   'isUpper' to identify upper case letters. -}

constructorname :: Parser String
constructorname = undefined

{- Here are some tests that your 'constructorname' parser should pass:

     runParser constructorname "plus"  == Nothing
     runParser constructorname "x"     == Nothing
     runParser constructorname ""      == Nothing
     runParser constructorname "Plus"  == Just "Plus"
     runParser constructorname "S"     == Just "S"
     runParser constructorname ""      == Nothing
     runParser constructorname "plu s" == Nothing
     runParser constructorname "123"   == Nothing -}

{- 1 MARK -}


{- 4.5 Parsing patterns.

   A pattern is either:

     - a variable name; or
     - a constructor name; or
     - a constructor name followed by a comma separated list of patterns, in
       parentheses.

   For example:

         Cons(Z,xs)

   Write a parser for patterns. -}

pat :: Parser Pat
pat =  undefined

{- 3 MARKS -}


{- 4.6 Parsing expressions

   An expression is either:

     - a variable name; or
     - a constructor name; or
     - a variable name followed by a comma separated list of expressions, in
       parentheses; or
     - a constructor name followed by a comma separated list of
       expressions, in parentheses.

   For example:

        append(Cons(Z,Nil),xs)

   Write a parser for expressions. This will be very similar to the
   parser for patterns above, so it is worth fewer marks. -}

expr :: Parser Exp
expr = undefined

{- 2 MARKS -}


{- 4.7 Parsing Equations.

   The concrete syntax for individual equations looks like:

      plus(S(x),y) = S(plus(x,y));

   Points to note:
     1. An equation starts with a lower-cased identifier; then
     2. a list of patterns in parentheses, separated by commas
     3. an equals sign
     4. an expression
     5. a semicolon

   We will write our parser so that it parses each equation in the
   program individually. The elaboration phase below will turn these
   lists of equations into actual 'Program's. -}

data Equation
  = MkEqn { eqnName :: String
          , eqnPats :: [Pat]
          , eqnExp  :: Exp
          }
  deriving Show

{- Using the 'pat' and 'expr' parsers you wrote above, write a 'Parser'
   for equations: -}

equation :: Parser Equation
equation = undefined

{- 3 MARKS -}


{- 4.8 Parsing lists of Equations.

   The final stage of parsing is a parser for lists of equations,
   separated by optional whitespace.
-}

equations :: Parser [Equation]
equations = undefined

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- 4.9 TEST: Syntactic sugar for numeric literals

   Extend your parsers for patterns and expressions to parse numeric
   literals. That is, instead of having to write 'S(S(S(Z)))', a GHOUL
   programmer should be able to write '3'. This should work in both
   patterns and in expressions.

   We have given you the 'number' parser at the bottom of this file to
   parse numbers. You should extend your 'pat' and 'expr' parsers to
   use this parser and turn the integers it returns into 'Pat's or
   'Exp's as appropriate. -}
{- 5 MARKS -}
{----------------------------------------------------------------------}



{--------------------------------------------------------------------}
{- Part 2 : ELABORATION                                             -}
{--------------------------------------------------------------------}

{- Given a list of equations, turn it into a program by grouping all the
   equations with the same name together, and checking that they all
   have the same arity. -}

{- 4.10 Elaboration.

   Write the function 'elaborate' that takes a list of equations and
   produces a 'Program'.

   HINT: Use 'groupBy' to group the equations into groups with the
   same name. Then write a function that takes a list of equations of
   the same name and turns them into a proper definition by checking
   that they all have the same number of patterns. -}

elaborate :: [Equation] -> Maybe Program
elaborate = undefined

{- 6 MARKS -}



{- 4.11 Scope checking.

   Not needed for basic GHOUL.

   GHOUL programs can go wrong in lots of ways -- variables may not be
   defined, functions may not be defined, functions may be called with
   arguments that they are not expecting, and so on. Fixing all of
   these is a surprising amount of work. In this question, you will
   implement a simple checker that checks whether everything
   referenced by name actually exists.

   For each equation, check that the variables used on the right-hand
   side are only the ones that are mentioned in the pattern on the
   left-hand side. Also check that all the function names used in the
   program are actually defined, and that there is a main function
   definition, with arity 0. -}

scopeCheck :: Program -> Bool
scopeCheck program = True -- YOU WRITE THIS.

{- 6 MARKS -}

{----------------------------------------------------------------------}
{- Part 3 : EXECUTION                                                 -}
{----------------------------------------------------------------------}

{- Execution of GHOUL programs results in values, which are defined to
   be constructors applied to lists of values: -}

data Val = VC String [Val]
         deriving (Show, Eq)

{- GHOUL programs have variables in them. To keep track of what each
   variable means by during execution, we use environments. An
   environment is a list containing pairs of names that are attached
   to values: -}

type Env = [(String, Val)]

{----------------------------------------------------------------------}
{- Part 3(a) : IMPLEMENTING PATTERN MATCHING                          -}
{----------------------------------------------------------------------}

{- Execution of a GHOUL program alternates between choosing which
   equation to apply, and then evaluating the right-hand side of that
   equation. Choosing which equation to apply is accomplished using
   pattern matching.

   The essence of pattern matching is to produce a mapping from
   variable names to values by matching a pattern against a compound
   value. To help with pattern matching, we use 'Matcher's: things
   that transform environments, but may fail: -}

newtype Matcher a = MkMatcher (Env -> Maybe (a, Env))

{- To test the matching functions that you will write below, use
   'runMatcher', which runs a matcher starting with an empty
   environment and returns the resulting value and environment. -}

runMatcher :: Matcher a -> Env -> Maybe (a, Env)
runMatcher (MkMatcher m) = m

{- 'Matcher' is a Functor, Applicative and Alternative, which will help
   you write the GHOUL-specific matching implementations
   below. 'Matcher' can also be given a 'Monad' structure, but it is
   possible to write a matcher without using that fact. You can
   implement 'Monad Matcher' for yourself if you think it will help. -}

instance Functor Matcher where fmap f m = pure f <*> m

instance Applicative Matcher where
  pure a = MkMatcher (\env -> Just (a, env))
  mf <*> ma = MkMatcher (\env -> do (f, env')  <- runMatcher mf env
                                    (a, env'') <- runMatcher ma env'
                                    return (f a, env''))

instance Alternative Matcher where
  empty     = MkMatcher (\env -> empty)
  m1 <|> m2 = MkMatcher (\env -> runMatcher m1 env <|> runMatcher m2 env)

{- We will be matching lists of patterns against lists of values. The
   obvious way to do this is to use the 'zip' function from the
   Haskell library. However, if the input lists are different lengths,
   then zip only returns a list of length the shorter of the two. So
   we define our own 'zipChecked' function that uses a 'Maybe' to
   signal when the lists are different lengths. -}

zipChecked :: [a] -> [b] -> Maybe [(a,b)]
zipChecked xs ys = go xs ys []
  where go []     []     zs = Just (reverse zs)
        go (x:xs) (y:ys) zs = go xs ys ((x,y):zs)
        go _      _      _  = Nothing

{- The basic operation that make 'Matcher's special is the ability to
   bind variables to values. This is the basic step in pattern
   matching. 'bindVar' takes a variable name and a value, and returns
   the 'Matcher' that performs the action of binding that variable to
   that value. Note that if the named variable already has a binding,
   then 'bindVar x v' signals failure by returning 'Nothing'. -}

bindVar :: String -> Val -> Matcher ()
bindVar x v =
  MkMatcher (\env -> case lookup x env of
                       Nothing -> Just ((), (x,v):env)
                       Just _  -> Nothing)

{- 4.12 Matching patterns and lists of patterns.

        Write the functions 'matchPat' and 'matchPats'.

   'matchPat' takes a pair of a pattern and a value and returns a
   Matcher. These ought to implement pattern matching:

     - matching a variable against any value binds the variable to the
       value.

     - matching a constructor pattern against a value checks that the
       value has a constructor with the same name, and that the
       sub-patterns match all of the sub-values.

   'matchPats' should take a list of patterns and a list of values,
   and return a Matcher generated by matching all the pairs. If the
   two lists have different lengths, then matching ought to fail. -}

matchPat :: (Pat, Val) -> Matcher ()
matchPat = undefined

matchPats :: [Pat] -> [Val] -> Matcher ()
matchPats = undefined

{- 3 MARKS -}

{- 4.13 Matching individual clauses.

   Write a function that takes a clause (i.e. a pair of a list of
   patterns and an expression) and a list of values and that returns a
   Matcher that returns the expression as its result. -}

matchClause :: ([Pat], Exp) -> [Val] -> Matcher Exp
matchClause = undefined

{- 1 MARK -}

{- 4.14 Matching clauses.

   The final piece of the pattern matching machinery takes a list of
   clauses (as taken from a definition), and a list of values, and
   returns the matcher that searches for a matching clause. -}

matchClauses :: [([Pat], Exp)] -> [Val] -> Matcher Exp
matchClauses = undefined

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- 4.15 TEST: Repeated variables in patterns

   The basic version of GHOUL does not allow definitions like the
   following, where we repeat a variable name in the patterns to
   indicate that both arguments should be equal.

      isEqual(x,x) = True;
      isEqual(x,y) = False;

   This restriction is enforced by the 'bindVar' function that fails
   if a variable is matched more than once. Write a new version of
   bindVar that allows repeated binding of a variable, as long as all
   values matched are equal. -}

bindVarAllowRepeats :: String -> Val -> Matcher ()
bindVarAllowRepeats x v = undefined

{- 2 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- 4.16 TEST: Catch-all patterns

   GHOUL does not allow catch-all patterns that do not bind a variable
   like Haskell's '_'. Adjust the Pat datatype, your pattern parser
   'pat', and the 'matchPat' function to make the following kind of
   definition work:

       const1(_) = S(Z);
-}
{- 3 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 3(b): EVALUATION OF EXPRESSIONS                               -}
{----------------------------------------------------------------------}

{- Evaluation of expressions in the context of some program and some
   environment is modelled using the 'Eval' data type. The 'Eval' type
   offers three services:

     1) The ability to look at the current program ('getPrg', below)
     2) The ability to look at the current environment ('getEnv', below)
     3) The ability to report failed execution ('empty', below)
-}

newtype Eval a = MkEval (Program -> Env -> Maybe a)

{- To 'run' some evaluation, use the 'runEval' function that runs an
   evaluation with a given program and environment: -}

runEval :: Eval a -> Program -> Env -> Maybe a
runEval (MkEval e) = e

{- 'Eval' supports the Monad operations 'return' and '>>=', which should
   not be surprising since it is the combination of the 'Reader'
   monad, and the 'Maybe' monad. As a consequence it also supports the
   'Functor' and 'Applicative' interfaces: -}

instance Monad Eval where
  return x = MkEval (\prg env -> Just x)
  MkEval e >>= k =
    MkEval (\prg env -> e prg env >>= \a -> runEval (k a) prg env)

instance Functor Eval where
  fmap f (MkEval e) = MkEval (\prg env -> fmap f (e prg env))

instance Applicative Eval where
  pure = return
  ef <*> ea = do f <- ef; a <- ea; return (f a)

{- 'Eval' also supports the 'Alternative' interface: 'empty' records
   failure, and '<|>' means 'try this, and if that fails, try that'. -}

instance Alternative Eval where
  empty = MkEval (\_ _ -> empty)
  e1 <|> e2 =
    MkEval (\prg env -> runEval e1 prg env <|> runEval e2 prg env)

{- The two basic operations supported by the 'Eval' monad are the ones
   that access the current environment, 'getEnv', and access the
   program being executed, 'getPrg'. You will need to use these below
   in order to implement the parts of evaluation of expressions that
   require looking up names in the environment or the global program. -}

getEnv :: Eval Env
getEnv = MkEval (\prg env -> Just env)

getPrg :: Eval Program
getPrg = MkEval (\prg env -> Just prg)

{- The 'afterMatch' function links pattern matching and
   evaluation. Given a function:

      f : a -> Eval b

   that takes 'a's and produces evaluation results of type 'b', and a
   'Matcher' that returns 'a's:

      m : Matcher a

   then 'afterMatch f m', runs the matcher 'm' to generate a new
   environment and a value 'a' and then runs the evaluation 'f a' in
   the environment generated by the pattern match. The program
   definitions are then passed through, because they are global.

   Some examples. All of these use 'getEnv' as the evaluation to run
   after the pattern match, so the result we will see in each case is
   the environment that the evaluation run after the pattern match is
   executed in. Explanations follow each example.

       λ> let v = VC "Z" []
       λ> runEval (const getEnv `afterMatch` (pure ())) [] [("x", v)]
       Just []

   So the 'null' pattern match (represented by 'pure ()') means that
   after the match we get the empty environment, even though we
   started with the environment '[("x", v)]'.

       λ> runEval (const getEnv `afterMatch` (bindVar "y" v)) [] [("x", v)] 
       Just [("y",VC "Z" [])]

   A 'matcher' computation that binds a variable, 'bindVar "y" v',
   results in an environment with that variable bound, and ignores any
   bindings in the outer environment.

       λ> runEval (const getEnv `afterMatch` empty) [] [("x", v)] 
       Nothing

   Evaluation after a failing pattern match, 'empty', results in a
   failing evalution. -}

afterMatch :: (a -> Eval b) -> Matcher a -> Eval b
afterMatch f (MkMatcher m) =
  MkEval (\prg _ -> do (a, env) <- m []
                       runEval (f a) prg env)


{- 4.16 Applying a definition to a list of values.

   Write the function 'applyDef' that applies a definition to a list
   of values. This must do the following:

     1. Check that the number of values given matches the arity of the
     definition.

     2. Find a clause of the definition that matches the values, if
     any.

     3. Evaluate the expression part of the matching clause in the new
     environment created by pattern matching.

   Evaluation of expressions is performed by the 'eval' function
   below. Use the 'afterMatch' function above to link matching and
   evaluation. -}

applyDef :: Definition -> [Val] -> Eval Val
applyDef = undefined

{- 2 MARKS -}



{- 4.17 Evaluating programs

   The final step in implementing a GHOUL interpreter is in the
   implementation of 'eval' for expressions. Implement this function,
   following this informal specification:

     1) constructor applications 'EC' evaluate all their arguments to
     values, and return a 'VC' value with the given constructor name
     applied to all the values.

     2) variables 'EV' are evaluated by looking up their value in the
     current environment.

     3) function applications 'EA' are evaluated by looking up the
     definition associated with the function name, evaluating all the
     arguments to values, and the applying that definition to those
     values. -}

eval :: Exp -> Eval Val
eval = undefined


evalProgram :: Program -> Maybe Val
evalProgram prg = runEval (eval (EA "main" [])) prg []

{- 4 MARKS -}


{----------------------------------------------------------------------}
{- Part 4: EXTRA FEATURES                                             -}
{----------------------------------------------------------------------}

{- 4.18 Partial Application

   GHOUL as described above is not a Higher-order language, despite
   the name. One way to extend GHOUL to be higher-order is to allow
   partial application of functions -- whenever too few arguments are
   given to a function, then instead of failing, we return a special
   value that represents a function application with too few
   arguments. Such a value can then be passed around and applied to
   the rest of its arguments later on.

   To implement this feature, you will need to:

       - Extend the datatype of values 'Val' to include partially
         applied definitions; i.e. definitions plus lists of values.

       - Extend 'applyDef' so that it does not fail if a definition is
         applied to too few values, but instead generates a partially
         applied value.

       - Write a new function 'applyVal' that applies a partially
         applied definition to some more values.

       - Alter the 'EA' case of eval to (a) try looking up the
         function name in the environment before in the programs; and
         (b) if it is found in the environment then it uses
         'applyVal'.

   If you have implemented scopeCheck above, then you will also need
   to extend the check that all functions that are used have been
   defined to allow functions that are named in patterns.

   Here is an example program that ought to work after your
   changes. Note that in the definition of plusTwo, plus is only
   applied to one argument, not the two it is expecting. -}

partialProg :: String
partialProg =
  " plus(Z,y) = y;\
  \ plus(S(x),y) = S(plus(x,y));\
  \ plusTwo() = plus(S(S(Z)));\
  \ apply(f,a) = f(a);\
  \ main() = apply(plusTwo(),S(S(Z)));"

{- 10 MARKS -}


{----------------------------------------------------------------------}
{- 4.19 TEST: Logging Effects

   Alter the GHOUL language so that there is a logging facility that
   records GHOUL values in a log as execution proceeds. You can use
   the 'show' function for the 'Val' datatype to turn GHOUL values
   into strings:

      show :: Val -> String

   You will need to:

    - Add a special 'print' operation to the GHOUL syntax that allows
      for printing to happen. You decide what the syntax should look
      like, and how it ought to behave (assuming that it does actually
      log values).

    - Alter the 'eval' and 'evalProgram' functions so that they
      return '([String], Maybe Val)', where the first part of the pair
      is the logged messages, and the second is the returned value (or
      Nothing for failure). You will need to alter the 'Eval' monad to
      record logging information.

    - You will need to change the definition of 'runGhoul' so that its
      type is:

         runGHOUL :: String -> Maybe ([String], Maybe Val)


   You may find the LogAndFail example in Lecture 16 useful.

   So that we know what you've done to add logging, please give a
   short list of the changes you've made below. -}

{- 10 MARKS -}
{----------------------------------------------------------------------}



{--------------------------------------------------------------------}
{- APPENDIX : PARSER COMBINATORS                                    -}
{--------------------------------------------------------------------}

{- Here is the code for the parser combinators you should use to
   implement your GHOUL parser. Please consult this code to help you
   write your parser, but do not alter it. -}

instance Functor Parser where
  fmap f (MkParser p) =
    MkParser (fmap (fmap (\(a,s) -> (f a,s))) p)

instance Applicative Parser where
  pure x = MkParser (\s -> [(x,s)])
  MkParser pf <*> MkParser pa =
    MkParser (\s -> [ (f a, s2) | (f, s1) <- pf s
                                , (a, s2) <- pa s1 ])

instance Monad Parser where
  MkParser p >>= k =
    MkParser (\s -> [ (b,s2) | (a, s1) <- p s
                             , let MkParser p2 = k a
                             , (b, s2) <- p2 s1 ])

instance Alternative Parser where
  empty = MkParser (const [])
  MkParser p1 <|> MkParser p2 = MkParser (\s -> p1 s ++ p2 s)

char :: Parser Char
char = MkParser p
  where p []     = []
        p (c:cs) = [(c, cs)]

isChar :: Char -> Parser ()
isChar expected = do
  seen <- char
  if expected == seen then return () else empty

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  c <- char
  guard (p c)
  return c

string :: String -> Parser ()
string = mapM_ isChar

digit :: Parser Int
digit = do
  c <- char
  guard (isNumber c)
  return (digitToInt c)

number :: Parser Int
number =
  foldl (\l r -> l*10+r) 0 <$> some digit

space :: Parser ()
space = () <$ satisfies isSpace

spaces :: Parser ()
spaces = () <$ many space

identifier :: Parser String
identifier = (:) <$> satisfies isAlpha <*> many (satisfies isAlphaNum)

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p = pure [] <|> (:) <$> p <*> many (sep *> p)

