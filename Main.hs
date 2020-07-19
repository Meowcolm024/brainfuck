{-# LANGUAGE LambdaCase#-}
module Main where

import           Text.ParserCombinators.Parsec

data Op = Ml | Mr | Ci | Cr | Pr deriving Show
data Bf a = Atom a | Loop [Bf a] deriving Show
data Pointer = Pointer {pos :: Int, mem :: [Int]} deriving Show
type Act = Either (Pointer -> IO()) (Pointer -> Pointer)

instance Functor Bf where
    fmap f (Atom x) = Atom (f x)
    fmap f (Loop x) = Loop (fmap f <$> x)

main :: IO ()
main = print $ map toBfOp <$> regularParse
    parseExpr
    (getVal "++++++++[>++++++<-]>+.")

-- Parser

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

parseLoop :: Parser (Bf String)
parseLoop = Loop <$> (char '[' *> parseExpr <* char ']')

parseAtom :: Parser (Bf String)
parseAtom = Atom <$> (many1 . oneOf) "+-<>."

parseExpr :: Parser [Bf String]
parseExpr = many $ choice [try parseLoop, parseAtom]

-- Helper

getVal :: String -> String
getVal = filter (`elem` "+-<>[].")

toBfOp :: Bf String -> Bf [Op]
toBfOp (Atom x) = Atom (map opMap x)
toBfOp (Loop x) = Loop (map toBfOp x)

opMap :: Char -> Op
opMap = \case
    '>' -> Mr
    '<' -> Ml
    '+' -> Ci
    '-' -> Cr
    '.' -> Pr
    _   -> error "Impossible!"

-- Eval

act :: Op -> Act
act = \case
    Mr -> Right (\(Pointer p m) -> Pointer (p+1) m)
    Ml -> Right (\(Pointer p m) -> Pointer (p-1) m)
    Ci -> Right (\(Pointer p m) -> Pointer p (f p m))
    Cr -> Right (\(Pointer p m) -> Pointer p (f p m))
    Pr -> Left (\(Pointer p m) -> putChar (toEnum (m !! p) :: Char))
  where f a xs = let (lp, rp) = splitAt a xs in lp ++ [head rp +1] ++ tail rp

