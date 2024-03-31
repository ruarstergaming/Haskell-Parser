module Expr where
import Parsing
import System.Console.Haskeline

import Data.Char ( isDigit )
import BinaryTree
import Parsing (sqBlockString)
type Name = String


-- |Expression that will be evaluated by eval
data Expr = Add      Expr          Expr     --add two numbers or concatenate  two strings
          | Sub      Expr          Expr     --subtrace two numbers
          | Mult     Expr          Expr     --multiply two numbers
          | Div      Expr          Expr     --divide two numbers
          | Abs      Expr                   --find the absolute value of a number
          | Pow      Expr          Expr     --find value of an exponential
          | Mod      Expr          Expr     --find the modulus of given numbers
          | ToString Expr                   --convert a number to a string
          | ToInt    Expr                   --convert a string to an integer 
          | Val      Value                  --a value
          | Var      Name                   --a variable name
          | Input                           --recieve input from user
          | EQU      Expr          Expr     --check if both expressions are equal
          | NQU      Expr          Expr     --check if both expressions are not equal
          | LTH      Expr          Expr     --check if first expression is less than the second
          | GTH      Expr          Expr     --check if first expression is greater than the second
          | LEE      Expr          Expr     --check if first expression is less than or equal to the second
          | GRE      Expr          Expr     --check if first expression is greater than or equal to the second
          | FuncDef  String        Command  --function argument names and function commands
  deriving Show

instance Show Value where
  show (IntVal a)    = show a
  show (FloatVal a)  = show a
  show (StrVal a)    = a
  show (Error a)     = a
  show (Boolean a)   = show a
  show (Funct f)     = show f


-- |Command that is executed by process
data Command = Set          Name Expr                   -- assign an expression to a variable name
             | Print        Expr                        -- evaluate an expression and print the result
             | File         String                      -- load a file and execute commands within it.
             | Block        String                      -- List of commands
             | If           Expr Command Command        -- if-else statement
             | Repeat       Expr Command                -- repeat a command a given number of times
             | For          Expr Expr Command Command   -- for loop
             | While        Expr Command                -- while loop
             | Quit                                     -- Exit the system
             | ExecFunc     String Name                 -- Execute a function
             | NoCommand                                -- user did not enter a command
  deriving Show

-- |Value that is used in the script
data Value = IntVal         Int           --Integer
           | FloatVal       Float         --Float
           | StrVal         String        --String
           | Error          String        --Error
           | Boolean        Bool          --Boolean
           | Funct          Function      --Function

instance Eq Value where
       (==) (IntVal a)      (IntVal b)        = a == b
       (==) (IntVal a)      (FloatVal b)      = fromIntegral a == b
       (==) (FloatVal a)    (IntVal b)        = a == fromIntegral b
       (==) (FloatVal a)    (FloatVal b)      = a == b
       (==) (StrVal a)      (StrVal b)        = a == b
       (==) (Boolean a)     (Boolean b )      = a == b
       (==) _               _                 = False

instance Ord Value where
       compare (IntVal a)      (IntVal b)      = compare a b
       compare (IntVal a)      (FloatVal b)    = compare (fromIntegral a) b
       compare (FloatVal a)    (IntVal b)      = compare  a (fromIntegral b)
       compare (FloatVal a)    (FloatVal b)    = compare a b
       compare _               _               = error "not comparable" -- checked by eval where it is handled properly

-- |Name-value pair that represents a variable
newtype Variable = Variable (Name,Value)

instance Eq Variable where
       (==) (Variable (n1,_)) (Variable (n2,_))         = n1==n2

instance Ord Variable where
       compare (Variable (n1,_)) (Variable (n2,_))      = compare n1 n2


data Function = Function {
                args    ::    [Name],
                commands::    [Command]
}

instance Show Function where
       show (Function a c) = show a


-- | Evaluate an expression based on given variables and return the result
eval :: BTree Variable -> -- Variable name to value mapping
                  Expr -> -- Expression to evaluate
           Maybe Value    -- Result (if no errors such as missing variables)
eval vars (Val x)         = Just x -- for values, just give the value directly
eval vars Input           = Just (Error "cannot evaluate input that has not been given") --input is given during process

{-
       Evaluate value of variable
-}
eval vars (Var x) = case getElem vars (Variable (x,IntVal 0)) of
                         Just (Variable (_,v))       -> Just v
                         _                           -> Just (Error ("Variable not in scope: "++x))
{-
       Evaluate addition or concatenation
-}
eval vars (Add x y) = case (eval vars x, eval vars y) of
                           (Just (IntVal x),Just (IntVal y))          -> Just (IntVal (x + y))
                           (Just (IntVal x),Just (FloatVal y))        -> Just (FloatVal (fromIntegral x + y))
                           (Just (FloatVal x),Just (IntVal y))        -> Just (FloatVal (x + fromIntegral y))
                           (Just (FloatVal x),Just (FloatVal y))      -> Just (FloatVal (x + y))
                           (Just (StrVal x),Just (StrVal y))          -> Just (StrVal (x ++ y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'+' operator must be used between two numbers or two strings")

{-
       Evaluate subtraction
-}
eval vars (Sub x y) = case (eval vars x, eval vars y) of
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           (Just (IntVal x),Just (IntVal y))          -> Just (IntVal (x - y))
                           (Just (IntVal x),Just (FloatVal y))        -> Just (FloatVal (fromIntegral x - y))
                           (Just (FloatVal x),Just (IntVal y))        -> Just (FloatVal (x - fromIntegral y))
                           (Just (FloatVal x),Just (FloatVal y))      -> Just (FloatVal (x - y))
                           _                                          -> Just (Error "'-' operator must be used between two numbers")

{-
       Evaluate multiplication
-}
eval vars (Mult x y) = case (eval vars x, eval vars y) of
                            (Just (IntVal x),Just (IntVal y))         -> Just (IntVal (x * y))
                            (Just (IntVal x),Just (FloatVal y))       -> Just (FloatVal (fromIntegral x * y))
                            (Just (FloatVal x),Just (IntVal y))       -> Just (FloatVal (x * fromIntegral y))
                            (Just (FloatVal x),Just (FloatVal y))     -> Just (FloatVal (x * y))
                            (Just (Error e),_)                        -> Just (Error e)
                            (_,Just (Error e))                        -> Just (Error e)
                            _                                         -> Just (Error "'*' operator must be used between two numbers")


{-
       Evaluate division
-}
eval vars (Div x y) = case (eval vars x, eval vars y) of
                           (Just (IntVal x),Just (IntVal y))          -> Just (FloatVal (fromIntegral x / fromIntegral y))
                           (Just (IntVal x),Just (FloatVal y))        -> Just (FloatVal (fromIntegral x / y))
                           (Just (FloatVal x),Just (IntVal y))        -> Just (FloatVal (x / fromIntegral y))
                           (Just (FloatVal x),Just (FloatVal y))      -> Just (FloatVal (x / y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'/' operator must be used between two numbers")

{-
       Evaluate |...| (Abs)
-}
eval vars (Abs e) = case eval vars e of
                         Just (IntVal y)                              -> Just (IntVal (abs y))
                         Just (FloatVal y)                            -> Just (FloatVal (abs y))
                         Just (Error e)                               -> Just (Error e)
                         _                                            -> Just (Error "'|...|' operator must be used on a number")

{-
       Evaluate ^ (Power)
-}
eval vars (Pow x y) = case (eval vars x, eval vars y) of
                           (Just (IntVal x),Just (IntVal y))          -> Just (IntVal (x ^ y))
                           (Just (IntVal x),Just (FloatVal y))        -> Just (FloatVal (fromIntegral x ** y))
                           (Just (FloatVal x),Just (IntVal y))        -> Just (FloatVal (x ** fromIntegral y))
                           (Just (FloatVal x),Just (FloatVal y))      -> Just (FloatVal (x ** y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'^' operator must be used between two numbers")

{-
       Evaluate % (Mod)
-}
eval vars (Mod x y) = case (eval vars x, eval vars y) of
                           (Just (IntVal x),Just (IntVal y))          -> Just (IntVal (x `mod` y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'%' operator must be used between two Integers")

eval vars (EQU x y) = case (eval vars x, eval vars y) of
                           (Just x,Just y)                            -> Just ( Boolean (x == y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "Not a valid boolean expression")

eval vars (NQU x y) = case (eval vars x, eval vars y) of
                           (Just x,Just y)                            -> Just ( Boolean (x /= y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "Not a valid boolean expression")

eval vars (LTH x y) = case (eval vars x, eval vars y) of
                           (Just x,Just y)                            -> Just ( Boolean (x < y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'Not a valid boolean expression")

eval vars (GTH x y) = case (eval vars x, eval vars y) of
                           (Just x,Just y)                            -> Just ( Boolean (x > y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'Not a valid boolean expression")

eval vars (LEE x y) = case (eval vars x, eval vars y) of
                           (Just x,Just y)                            -> Just ( Boolean (x <= y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'Not a valid boolean expression")

eval vars (GRE x y) = case (eval vars x, eval vars y) of
                           (Just x,Just y)                            -> Just ( Boolean (x >= y))
                           (Just (Error e),_)                         -> Just (Error e)
                           (_,Just (Error e))                         -> Just (Error e)
                           _                                          -> Just (Error "'Not a valid boolean expression")

eval vars (FuncDef ns (Block cs))  = Just (Funct (Function (split ',' ns) (map stringToCommand (split ';' cs))))
eval vars (FuncDef ns c)           = Just (Funct (Function (split ',' ns) [c]))




{-
       Evaluate ToString
-}
eval vars (ToString x) = Just (StrVal (show (removeMaybe(eval vars x))))

{-
       Evaluate ToInt
-}
eval vars (ToInt x) = case eval vars x of
                        Just (StrVal s) -> if all isDigit s then
                           Just (IntVal (digitsToInt s)) else
                             Nothing
                        _ -> Nothing

-- |Converts a list of numeric characters to a single integer
digitsToInt :: [Char] -> Int
digitsToInt ds = read ds ::Int

-- |Converts a list of numeric characters to a single floating point number
digitsToFloat :: [Char] -> Float
digitsToFloat [] = error "No numbers given"
digitsToFloat ds = read ds ::Float

-- |Parses commands from the user
pCommand :: Parser Command
pCommand =      do t <- letter
                   ts <- many letter
                   space
                   string "args("
                   space
                   as <- argNameString
                   space
                   char ')'
                   space
                   return (ExecFunc (t:ts) as )

            ||| do t <- letter
                   ts <- many letter
                   space
                   char '='
                   space
                   Set (t:ts) <$> pExpr

            ||| do space
                   string "print"
                   space
                   Print <$> pExpr

            ||| do space
                   string "quit" ||| string "exit"
                   space
                   return Quit

            ||| do space
                   string "file"
                   space
                   char '\0'
                   t <- many printable
                   char '\0'
                   space
                   return (File t)
            ||| do space
                   string "if("
                   space
                   e <- pExpr
                   space
                   char ')'
                   space
                   t <-pCommand
                   space
                   string "else"
                   space
                   If e t <$> pCommand
            ||| do space
                   string "if("
                   space
                   e <- pExpr
                   space
                   char ')'
                   space
                   t <-pCommand
                   space
                   return (If e t NoCommand)
            ||| do space
                   char '{'
                   space
                   cs <- blockString
                   space
                   char '}'
                   space
                   return (Block cs)
            ||| do space
                   char '['
                   space
                   cs <- sqBlockString
                   space
                   char ']'
                   space
                   return (Block cs)
            ||| do space
                   string "repeat"
                   space
                   n <- pExpr
                   space
                   Repeat n <$> pCommand
            ||| do space
                   string "for("
                   space
                   i <- pExpr
                   space
                   char ';'
                   space
                   t <- pExpr
                   space
                   char ';'
                   space
                   m <- pCommand
                   space
                   char ')'
                   space
                   c <- pCommand
                   space
                   return (For i t m c)
            ||| do space
                   string "while("
                   space
                   e <- pExpr
                   space
                   char ')'
                   space
                   While e <$> pCommand

            ||| do string ""
                   return NoCommand

-- |Parses expressions from the user
pExpr :: Parser Expr
pExpr = do t <- pTerm
       --parse '+' operator
           do  space
               char '+'
               space
               Add t <$> pExpr
       --parse '-' operator
            ||| do space
                   char '-'
                   space
                   Sub t <$> pExpr
       --parse '|...|' operator
            ||| do space
                   char '|'
                   space
                   e <- pExpr
                   space
                   char '|'
                   space
                   return (Abs e)
       --parse '==' operator
            ||| do space
                   string "=="
                   space
                   EQU t <$> pExpr
       --parse '!=' operator
            ||| do space
                   string "!="
                   space
                   NQU t <$> pExpr
       --parse '>=' operator
            ||| do space
                   string ">="
                   space
                   GRE t <$> pExpr
       --parse '<=' operator
            ||| do space
                   string "<="
                   space
                   LEE t <$> pExpr
       --parse '<' operator
            ||| do space
                   string "<"
                   space
                   LTH t <$> pExpr
       --parse '>' operator
            ||| do space
                   string ">"
                   space
                   GTH t <$> pExpr



                 ||| return t

-- |Parses factors from the user
pFactor :: Parser Expr
pFactor = do space
             string "input"
             space
             return Input
       --parse a float
         ||| do d <- digit ||| char '-'
                ds <- many digit
                char '.'
                n <- digit
                ns <- many digit
                space
                return (Val (FloatVal (digitsToFloat ( (d:ds) ++ "." ++ (n:ns) ) )))
       --parse an integer
         ||| do d <- digit ||| char '-'
                ds <- many digit
                space
                return (Val (IntVal (digitsToInt (d:ds))))
       --parse a string comtaining arguments
         ||| do string "args("
                space
                as <- argNameString
                space
                char ')'
                space
                FuncDef as <$> pCommand
       --parse an expression inside brackets
         ||| do  space
                 char '('
                 space
                 e <- pExpr
                 space
                 char ')'
                 space
                 return e
       --parse a string that was given in quotes
         ||| do space
                char '\0'
                e <- many printable
                char '\0'
                space
                return (Val (StrVal e))
       --parse toString function
         ||| do space
                string "toString("
                space
                e <- pExpr
                space
                char ')'
                return(ToString e)
       --parse toInt function
         ||| do string "toInt("
                space
                e <- pExpr
                space
                char ')'
                return(ToInt e)
       --parse a variable name
         ||| do v <- many letter
                return (Var v)

-- |Parses terms from the user
pTerm :: Parser Expr
pTerm = do f <- pFactor
       --parse '*' operator
           do space
              char '*'
              space
              e <- pTerm
              space
              return (Mult f e)
       --parse '/' operator
            ||| do space
                   char '/'
                   space
                   e <- pTerm
                   space
                   return (Div f e)
       --parse '%' operator
            ||| do space
                   char '%'
                   space
                   Mod f <$> pTerm
       --parse '^' operator
            ||| do char '^'
                   space
                   Pow f <$> pExpr
                 ||| return f

-- |When given a just value, returns value
removeMaybe :: Maybe a ->  a
removeMaybe (Just a) = a
removeMaybe Nothing  = error "No value"

stringToCommand          :: String -> Command
stringToCommand s        = case parse pCommand (replaceChars s '"' '\0') of
                              [(cmd, "")] -> cmd
                              _           -> Print (Val(Error "parse error"))

-- |Replaces all instances of a character in a string with another
replaceChars :: String -> Char -> Char -> String
replaceChars s c1 c2 = map (\c -> if c==c1 then c2 else c) s

{- |Splits a string on a given character
code is a modified version of "lines" from prelude - found at: 
https://www.haskell.org/onlinereport/standard-prelude.html-}
split              :: Char -> String -> [String]
split c ""         =  []
split c s          =  let (l, s') = break (== c) s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> split c s''