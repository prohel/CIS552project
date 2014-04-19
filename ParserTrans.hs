{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module ParserTrans (GenParser, Parser, 
                   getC,
                   choose,
                   (<|>),
                   satisfy,
                   doParse,  
                   ) where

newtype GenParser e a = P ([e] -> [(a, [e])])

type Parser a = GenParser Char a

doParse :: GenParser e a  -> [e] -> [(a,[e])]
doParse (P p) s = p s

instance Monad (GenParser e) where
   p1 >>= fp2 = P (\cs -> do (a,cs') <- doParse p1 cs 
                             doParse (fp2 a) cs') 

   return x   = P (\cs -> [ (x, cs) ])

   fail _     = P (\_ ->  [ ])

instance Functor (GenParser e) where
   fmap f p = do x <- p
                 return (f x)

-- | Return the next character
getC :: GenParser e e 
getC = P (\cs -> case cs of
                (x:xs) -> [ (x,xs) ]
                []     -> [])

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e
satisfy p = do c <- getC
               if (p c) then return c else fail "End of input"

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]

 
