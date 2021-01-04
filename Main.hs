{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void)
import Data.Either (fromRight)
import Data.Foldable (foldlM)
import Lens.Micro (ix, (%~), (&))
import System.Environment (getArgs)
import System.IO
import Text.ParserCombinators.Parsec

type Op = Char
data Bf = Atom [Op] | Loop [Bf]
data Pointer = Pointer {pos :: Int, mem :: [Int]}
data Act = In (Pointer -> IO Pointer) | Out (Pointer -> IO ()) | Norm (Pointer -> Pointer)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then flushStr "Brain Fuck\n" >> runRepl
    else runOne (head args)

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
readExpr x = void $ walkBf (fromRight [] $ eval x) (Pointer 0 (replicate 64 0))

eval :: String -> Either ParseError [Bf]
eval = regularParse parseExpr . filter (`elem` "+-<>[].,")

-- Parser

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

parseLoop :: Parser Bf
parseLoop = Loop <$> (char '[' *> parseExpr <* char ']')

parseAtom :: Parser Bf
parseAtom = Atom <$> (many1 . oneOf) "+-<>.,"

parseExpr :: Parser [Bf]
parseExpr = many1 $ choice [try parseLoop, parseAtom]

-- Eval

act :: Op -> Act
act = \case
  '>' -> Norm (\(Pointer p m) -> Pointer (p + 1) m)
  '<' -> Norm (\(Pointer p m) -> Pointer (p - 1) m)
  '+' -> Norm (\(Pointer p m) -> Pointer p (update p (ascend True) m))
  '-' -> Norm (\(Pointer p m) -> Pointer p (update p (ascend False) m))
  '.' -> Out (\(Pointer p m) -> flushStr [toEnum (m !! p) :: Char])
  ',' ->
    In
      ( \(Pointer p m) ->
          (\op -> Pointer p (update p (const $ fromEnum op) m)) <$> getChar
      )
  _ -> error "Impossible!"
  where
    update i op xs = xs & ix i %~ op
    ascend True i = if i == 255 then 0 else i+1
    ascend False i = if i == 0 then 255 else i-1

walk :: Pointer -> Bf -> IO Pointer
walk pt (Atom ops) = foldlM (flip apply) pt (map act ops)
  where
    apply :: Act -> Pointer -> IO Pointer
    apply (Norm f) x = return $ f x
    apply (Out f) x = f x >> return x
    apply (In f) x = f x
walk pt lp@(Loop lps) =
  if (mem pt !! pos pt) == 0
    then return pt
    else walkBf lps pt >>= flip walk lp

walkBf :: [Bf] -> Pointer -> IO Pointer
walkBf = flip $ foldlM walk
