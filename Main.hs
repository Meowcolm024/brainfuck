{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad                  ( void )
import           System.Environment             ( getArgs )
import           System.IO
import           Text.ParserCombinators.Parsec

data Op = Ml | Mr | Ci | Cr | Pr | Ip deriving Show
data Bf a = Atom a | Loop [Bf a] deriving Show
data Pointer = Pointer {pos :: Int, mem :: [Int]} deriving Show
data Act = In (Pointer -> IO Pointer) | Out (Pointer -> IO ()) | Norm (Pointer -> Pointer)

main :: IO ()
main = do
  args <- getArgs
  if null args then flushStr "Brain Fuck\n" >> runRepl else runOne (head args)

runRepl :: IO ()
runRepl = readPrompt "> " >>= readExpr >> runRepl

runOne :: String -> IO ()
runOne arg = do
  handle <- openFile arg ReadMode
  hGetContents handle >>= readExpr
  hClose handle

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

readExpr :: String -> IO ()
readExpr x = void $ walkBf (fromEither $ eval x) (Pointer 0 (replicate 32 0))
 where
  fromEither (Right r) = r
  fromEither (Left  _) = []

eval :: String -> Either ParseError [Bf [Op]]
eval = (map toBfOp <$>) . regularParse parseExpr . filter (`elem` "+-<>[].,")

-- Parser

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

parseLoop :: Parser (Bf String)
parseLoop = Loop <$> (char '[' *> parseExpr <* char ']')

parseAtom :: Parser (Bf String)
parseAtom = Atom <$> (many1 . oneOf) "+-<>.,"

parseExpr :: Parser [Bf String]
parseExpr = many1 $ choice [try parseLoop, parseAtom]

-- Eval

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
  ',' -> Ip
  _   -> error "Impossible!"

act :: Op -> Act
act = \case
  Mr -> Norm (\(Pointer p m) -> Pointer (p + 1) m)
  Ml -> Norm (\(Pointer p m) -> Pointer (p - 1) m)
  Ci -> Norm (\(Pointer p m) -> Pointer p (f p (+ 1) m))
  Cr -> Norm (\(Pointer p m) -> Pointer p (f p (\x -> x - 1) m))
  Pr -> Out (\(Pointer p m) -> flushStr [toEnum (m !! p) :: Char])
  Ip -> In
    (\(Pointer p m) ->
      (\op -> Pointer p (f p (const $ fromEnum op) m)) <$> getChar
    )
 where
  f a op xs = let (lp, rp) = splitAt a xs in lp ++ [op $ head rp] ++ tail rp

walk :: Pointer -> Bf [Op] -> IO Pointer
walk pt (Atom ops) = chainF (map act ops) pt
 where
  fx :: Act -> Pointer -> IO Pointer
  fx (Norm f) x = return $ f x
  fx (Out  f) x = f x >> return x
  fx (In   f) x = f x
  chainF :: [Act] -> Pointer -> IO Pointer
  chainF []       x = return x
  chainF (f : fs) x = fx f x >>= chainF fs
walk pt lp@(Loop lps) =
  if (mem pt !! pos pt) == 0 then return pt else walkBf lps pt >>= flip walk lp

walkBf :: [Bf [Op]] -> Pointer -> IO Pointer
walkBf []       x = return x
walkBf (b : bs) x = walk x b >>= walkBf bs
