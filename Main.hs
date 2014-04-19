-- Advanced Programming, HW 5
-- by <NAME1> <PENNKEY1>
--    <NAME2> <PENNKEY2>

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where 

import Control.Monad
import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>),(<>))
import qualified Text.PrettyPrint as PP

import Parser
import ParserCombinators

import ParserPrime
import ParserPrimeCombinators

import Test.HUnit

type Variable = String
 
data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)
 
data Expression =
    Var Variable
  | Val Value  
  | Op  Bop Expression Expression
  deriving (Show, Eq)
 
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt        
  | Ge       
  | Lt       
  | Le       
  deriving (Show, Eq)

data Statement =
    Assign Variable Expression          
  | If Expression Statement Statement
  | While Expression Statement       
  | Sequence Statement Statement        
  | Skip
  deriving (Show, Eq)

-- Problem 0
---------------------------------------------

wFact :: Statement
wFact = Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1)))))))))

class PP a where
  pp :: a -> Doc

oneLine :: PP a => a -> IO ()
oneLine = putStrLn . PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

indented :: PP a => a -> IO ()
indented = putStrLn . PP.render . pp

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

instance PP Value where
  pp (BoolVal True)  = PP.text "true"
  pp (BoolVal False) = PP.text "false"
  pp (IntVal x)      = PP.int x

instance PP Expression where
  pp (Var x)        = PP.text x
  pp (Val x)        = pp x
  pp (Op bop e1 e2) = pp_paren e1 <+> pp bop <+> pp_paren e2 where
    pp_paren (Var v) = PP.text v
    pp_paren (Val v) = pp v
    pp_paren ex      = PP.parens $ pp ex

instance PP Statement where
  pp (Assign v ex)    = PP.text v <+> PP.text ":=" <+> pp ex
  pp Skip             = PP.text "skip"
  pp (If ex s1 s2)    = PP.vcat [ PP.text "if" <+> pp ex <+> PP.text "then",
                                  PP.nest 2 (pp s1),
                                  PP.text "else",
                                  PP.nest 2 (pp s2),
                                  PP.text "endif" ]
  pp (While s ex)     = PP.vcat [ PP.text "while" <+> pp s <+> PP.text "do",
                                  PP.nest 2 (pp ex),
                                  PP.text "endwhile" ]
  pp (Sequence e1 e2) = PP.vcat [ pp e1 <> PP.semi, pp e2]

display :: PP a => a -> String
display = show . pp

-- Simple tests 

oneV,twoV,threeV :: Expression
oneV   = Val (IntVal 1)
twoV   = Val (IntVal 2)
threeV = Val (IntVal 3)

t0 :: Test
t0 = TestList [display oneV ~?= "1",
      display (BoolVal True) ~?= "true",        
      display (Var "X") ~?= "X",
      display (Op Plus oneV twoV) ~?= "1 + 2",
      display (Op Plus oneV (Op Plus twoV threeV)) ~?= "1 + (2 + 3)", 
      display (Op Plus (Op Plus oneV twoV) threeV) ~?= "(1 + 2) + 3",
      display (Assign "X" threeV) ~?= "X := 3",
      display Skip ~?= "skip"  ]

--- (Your own test cases go here...)
varA, varB, varX, varY, varZ :: Expression
varA = Var "A"
varB = Var "B"
varX = Var "X"
varY = Var "Y"
varZ = Var "Z"

exAss1, exAss2, exSeq :: Statement
exAss1 = Assign "A" varB
exAss2 = Assign "X" varY
exSeq = Sequence exAss1 exAss2

t0a :: Test
t0a = TestList [
      display (If (Op Gt varX varY) (Assign "X" varA) (Assign "X" varB)) 
        ~?= "if X > Y then\n  X := A\nelse\n  X := B\nendif",
      display (While (Op Gt varX oneV) exSeq)
        ~?= "while X > 1 do\n  A := B;\n  X := Y\nendwhile",
      display exSeq
        ~?= "A := B;\nX := Y"
      ]


t0b :: Test
t0b  = display (If (Val (BoolVal True)) Skip Skip) ~?=
       "if true then skip else skip endif"

t0b' :: Test
t0b' = display (If (Val (BoolVal True)) Skip Skip) ~?=
      "if true then\n  skip\nelse\n  skip\nendif"

------------------------------------------------------------------------
-- Problem 1

valueP :: Parser Value
valueP = intP <|> boolP

intP :: Parser Value
intP = liftM IntVal int

constP :: String -> a -> Parser a
constP s x = do _ <- string s
                return x

boolP :: Parser Value
boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

opP :: Parser Bop 
opP = choice [ constP "+" Plus,
      constP "-" Minus,
      constP "*" Times,
      constP "/" Divide,
      constP ">=" Ge,
      constP "<=" Le, 
      constP ">" Gt,
      constP "<" Lt]

varP :: Parser Variable
varP = many1 upper

wsP :: Parser a -> Parser a
wsP p = do x <- p
           _ <- many space
           return x

-- | parser for expression of val or var
vP :: Parser Expression
vP = (wsP $ liftM Val valueP) <|> (wsP $ liftM Var varP)

-- | parser for expression of op with parens
opParenP :: Parser Expression
opParenP = do _ <- wsP $ char '('
              e1 <- wsP opParenP <|> vP
              op <- wsP opP
              e2 <- wsP opParenP <|> vP
              _ <- wsP $ char ')'
              return $ Op op e1 e2

-- | parser for expression of op without parens
opNoParenP :: Parser Expression
opNoParenP = do e1 <- wsP opParenP <|> vP
                op <- wsP opP
                e2 <- wsP opParenP <|> vP
                return $ Op op e1 e2

exprP :: Parser Expression
exprP = opNoParenP <|> vP

t11 :: Test
t11 = TestList ["s1" ~: succeed (parse exprP "1 "),
                "s2" ~: succeed (parse exprP "1  + 2") ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

-- | My own test cases for exprP
ex1, ex2, ex3 :: Expression
ex1 = Op Plus (Val (IntVal 1)) (Val (IntVal 2))
ex2 = Op Minus ex1 (Val (IntVal 3))
ex3 = Op Plus ex2 (Op Plus (Var "X") (Var "Y"))

t11' :: Test
t11' = TestList [
       doParse exprP "1 " 
         ~?= [(Val (IntVal 1),"")],
       doParse exprP "1 + 2 "
         ~?= [(ex1,"")],
       doParse exprP "(  1 + 2 ) - 3"
         ~?= [(ex2,"")],
       doParse exprP "( ( 1 + 2) - 3 ) + ( X + Y ) "
         ~?= [(ex3,"")]]


assignP :: Parser Statement 
assignP = do v <- wsP varP
             _ <- wsP $ string ":="
             ex <- exprP
             return $ Assign v ex

skipP :: Parser Statement
skipP = wsP $ constP "skip" Skip

seqP :: Parser Statement
seqP = do s1 <- choice [ assignP, skipP, whileP, ifP]
          _ <- wsP $ string ";"
          s2 <- statementP
          return $ Sequence s1 s2

whileP :: Parser Statement
whileP = do ex <- between (wsP $ string "while") exprP (wsP $ string "do")
            st <- statementP
            _ <- wsP $ string "endwhile"
            return $ While ex st

ifP :: Parser Statement
ifP = do ex <- between (wsP $ string "if") exprP (wsP $ string "then")
         s1 <- statementP
         s2 <- between (wsP $ string "else") statementP (wsP $ string "endif")
         return $ If ex s1 s2

statementP :: Parser Statement
statementP = choice [ seqP, assignP, skipP, whileP, ifP]

-- | My own test cases for statementP
t120 :: Test
t120 = TestList [
       doParse statementP "X := (1 + 2) - 3"
         ~?= [(Assign "X" ex2,"")],
       doParse statementP "skip"
         ~?= [(Skip,"")],
       doParse statementP "X := Y; X := (1 + 2) - 3"
         ~?= [(Sequence (Assign "X" (Var "Y")) (Assign "X" ex2),"")],
       doParse statementP "while X > Y do A := B; X := Y endwhile"
         ~?= [(While (Op Gt (Var "X") (Var "Y")) exSeq,"")],
       doParse statementP "if X > Y then A := B else X := Y endif"
         ~?= [(If (Op Gt (Var "X") (Var "Y")) exAss1 exAss2,"")]]

t12 :: Test
t12 = TestList ["s1" ~: p "fact.imp",
                "s2" ~: p "test.imp", 
                "s3" ~: p "abs.imp" ,
                "s4" ~: p "times.imp" ] where
  p s = parseFromFile statementP s >>= succeed
    -- Or: p = succeed <=< parseFromFile statementP
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

testRT :: String -> Assertion
testRT filename = do 
   x <- parseFromFile statementP filename 
   case x of 
     Right ast -> case parse statementP (display ast) of
       Right ast' -> assert (ast == ast')
       Left _ -> assert False
     Left _ -> assert False                             

t13 :: Test
t13 = TestList ["s1" ~: testRT "fact.imp",
                "s2" ~: testRT "test.imp", 
                "s3" ~: testRT "abs.imp" ,
                "s4" ~: testRT "times.imp" ]


instance Arbitrary Value where 
  arbitrary = oneof [ liftM IntVal arbitrary, 
                      liftM BoolVal arbitrary ]

genVarName :: Gen String
genVarName = elements $ map (:[]) ['A'..'Z']

instance Arbitrary Bop where
  arbitrary = elements [Plus, Minus, Times, Divide, Gt, Ge, Lt, Le]

genArithOp :: Gen Bop
genArithOp = elements [Plus, Minus, Times, Divide]

genBoolOp :: Gen Bop
genBoolOp = elements [Gt, Ge, Lt, Le]


instance Arbitrary Expression where
  arbitrary = sized arbnE where
    arbnE n = frequency [ (10, liftM Var genVarName), 
                          (10, liftM Val arbitrary),
                          (n, liftM3 Op arbitrary (arbnE (n `div` 2)) 
                            (arbnE (n `div` 2))) ]                  

genArith :: Gen Expression
genArith = sized genArith_ where
  genArith_ n = frequency [ (12, liftM Var genVarName), 
                            (12, liftM (Val . IntVal) arbitrary),
                            (n, liftM3 Op genArithOp (genArith_ (n `div` 5))
                              (genArith_ (n `div` 5)))]

genBool :: Gen Expression
genBool = liftM3 Op genBoolOp genArith genArith

genLessSt :: (Int -> Gen Statement) -> Int -> Gen Statement
genLessSt gen n = gen (n `div` 4)

instance Arbitrary Statement where
  arbitrary = sized arbnS where
    arbnS n = 
      frequency [ (4, liftM2 Assign genVarName arbitrary),
                  (n, liftM3 If genBool (genLessSt arbnS n) (genLessSt arbnS n)), 
                  (n, liftM2 While genBool (genLessSt arbnS n)),
                  (n, liftM2 Sequence (genLessSt arbnS' n) (genLessSt arbnS n)),
                  (2, return Skip)]
    arbnS' n = 
      frequency [ (4, liftM2 Assign genVarName arbitrary),
                  (n, liftM3 If genBool (genLessSt arbnS n) (genLessSt arbnS n)), 
                  (n, liftM2 While genBool (genLessSt arbnS n)),
                  (2, return Skip)]

prop_roundtrip :: Statement -> Bool
prop_roundtrip s = case parse statementP (display s) of
  Right s'  -> s == s'
  Left _    -> False


------------------------------------------------------------------------
-- Problem 2

data Token = 
     TokVar String     -- variables
   | TokVal Value      -- primitive values
   | TokBop Bop        -- binary operators
   | Keyword String    -- keywords        
      deriving (Eq, Show)

keywords :: [ Parser Token ]
keywords = map (\x -> constP x (Keyword x)) 
             [ "(", ")", ":=", ";", "if", "then", "else",
             "endif", "while", "do", "endwhile", "skip" ]

type Lexer = Parser [Token]

lexer :: Lexer
lexer = sepBy1
        (liftM TokVal valueP <|>
         liftM TokVar varP   <|>
         liftM TokBop opP    <|>
         choice keywords)
        (many space)

t2 :: Test
t2 = parse lexer "X := 3" ~?= 
        Right [TokVar "X", Keyword ":=", TokVal (IntVal 3)]

isTokVar :: Token -> Bool
isTokVar (TokVar _) = True
isTokVar _ = False

isTokVal :: Token -> Bool
isTokVal (TokVal _) = True
isTokVal _ = False

isTokBop :: Token -> Bool
isTokBop (TokBop _) = True
isTokBop _ = False

isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _ = False

tokVar, tokVal, tokBop, keyword :: Parser' Token Token
tokVar = satisfy' isTokVar
tokVal = satisfy' isTokVal           
tokBop = satisfy' isTokBop
keyword = satisfy' isKeyword



varP' :: Parser' Token Variable
varP' = do x <- get'
           case x of
             TokVar s -> return $ s
             _        -> fail ""

intP' :: Parser' Token Value
intP' = do x <- get'
           case x of
             TokVal (IntVal y) -> return $ IntVal y
             _                   -> fail ""

constP' :: [Token] -> a -> Parser' Token a
constP' s x = do _ <- array s
                 return x

boolP' :: Parser' Token Value
boolP' = constP' [TokVal (BoolVal True)]  (BoolVal True) <||>
         constP' [TokVal (BoolVal False)]  (BoolVal False)

valueP' :: Parser' Token Value
valueP' = intP' <||> boolP'

--tokenP' :: Parser' Token Char
--tokenP' = do x <- choice keywords
--             return $ constP' [Keyword x] x

opP' :: Parser' Token Bop
opP' = choice' [ constP' [TokBop Plus] Plus,
                 constP' [TokBop Minus] Minus,
                 constP' [TokBop Times] Times,
                 constP' [TokBop Divide] Divide,
                 constP' [TokBop Ge] Ge,
                 constP' [TokBop Le] Le,
                 constP' [TokBop Gt] Gt,
                 constP' [TokBop Lt] Lt]

vP' :: Parser' Token Expression
vP' = (liftM Val valueP') <||> (liftM Var varP')

-- | parser for expression of op with parens
opParenP' :: Parser' Token Expression
opParenP' = do _ <- constP' [Keyword "("]  "("
               e1 <- opParenP' <||> vP'
               op <- opP'
               e2 <- opParenP' <||> vP'
               _ <- constP' [Keyword ")"]  ")"
               return $ Op op e1 e2

opNoParenP' :: Parser' Token Expression
opNoParenP' = do e1 <- opParenP' <||> vP'
                 op <- opP'
                 e2 <- opParenP' <||> vP'
                 return $ Op op e1 e2

exprP' :: Parser' Token Expression
exprP' = opNoParenP' <||> vP'

assignP' :: Parser' Token Statement 
assignP' = do v <- varP'
              _ <- constP' [Keyword ":="]  ":="
              ex <- exprP'
              return $ Assign v ex

skipP' :: Parser' Token Statement
skipP' = constP' [Keyword "skip"]  Skip

seqP' :: Parser' Token Statement
seqP' = do s1 <- choice' [ assignP', skipP', whileP', ifP']
           _ <- constP' [Keyword ";"]  ";"
           s2 <- statementP'
           return $ Sequence s1 s2

whileP' :: Parser' Token Statement
whileP' = do ex <- between' (constP' [Keyword "while"]  "while") exprP' (constP' [Keyword "do"]  "do")
             st <- statementP'
             _ <- constP' [Keyword "endwhile"]  "endwhile"
             return $ While ex st

ifP' :: Parser' Token Statement
ifP' = do ex <- between' (constP' [Keyword "if"]  "if") exprP' (constP' [Keyword "then"]  "then")
          s1 <- statementP'
          s2 <- between' (constP' [Keyword "else"]  "else") statementP' (constP' [Keyword "endif"]  "endif")
          return $ If ex s1 s2

statementP' :: Parser' Token Statement
statementP' = choice' [ seqP', assignP', skipP', whileP', ifP']

prop_groundtrip :: Statement -> Bool
prop_groundtrip s = case parse lexer (display s) of
  Right s'  -> case (parse' statementP' s') of
                  Right s'' -> display s == display s''
                  Left  _   -> False
  Left _    -> False

-----------------------------------------------------------------
-- A main action to run all the tests...

main :: IO () 
main = do _ <- runTestTT (TestList [ t0, 
                                     t11, t12, t13, 
                                     t2 ])
          return ()
