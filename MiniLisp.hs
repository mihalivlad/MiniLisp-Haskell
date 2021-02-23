module MiniLisp where

import Parser

data LispAtom = Number Int | Symbol String deriving (Eq, Show)
data LispValue = List [LispValue] | Atom LispAtom deriving (Eq, Show)

ws :: Parser String
ws = many ((char ' ') `orElse` (char '\n'))

between :: Parser a -> Parser b -> Parser c -> Parser c
between pHd pTl p = pHd `pThen` pMap fst (p `andThen` pTl)

ident :: Parser String
ident = pMap (\(a, b) -> a++b) (andThen (some alpha) (many (alpha `orElse` digit `orElse` satisfies (\c -> elem c ['?']))))

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p =(pMap (\(a,b)->[a]++b) (p `andThen` many (sep `pThen` p))) `orElse` (many (sep `pThen` p))

lisp :: Parser LispValue
lisp = pMap (\a -> Atom a) lispAtom `orElse` pMap (\a -> List a) lispList

lispAtom :: Parser LispAtom
lispAtom = ((pMap (\a -> Number a)) number) `orElse` ((pMap (\a -> Symbol a)) ident)

lispList :: Parser [LispValue] 
lispList = char '(' `pThen` (sepBy ws (pMap (\a -> Atom a) lispAtom)) `pThenFirst` char ')'