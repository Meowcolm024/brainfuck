{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad                  ( void )
import           Data.Either                    ( fromRight )
import           Data.Foldable                  ( foldlM )
import           Data.Vector                    ( (!)
                                                , Vector
                                                )
import qualified Data.Vector                   as V
import           System.Environment             ( getArgs )
import           System.IO
import           Text.ParserCombinators.Parsec

type Op = Char
data Bf = Atom [Op] | Loop [Bf]
data Pointer = Pointer
  { pos :: Int
  , mem :: Vector Int
  }
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
readExpr x = case eval x of
  Left err -> print err
  Right bf -> 
    void $ foldlM walk (Pointer 0 $ V.fromListN 128 (repeat 0)) bf

eval :: String -> Either ParseError [Bf]
eval = regularParse (parseExpr <|> (eof >> return [])) . filter (`elem` "+-<>[].,")

-- Parser

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "brainfuck"

parseLoop :: Parser Bf
parseLoop = Loop <$> (char '[' *> parseExpr <* char ']')

parseAtom :: Parser Bf
parseAtom = Atom <$> (many1 . oneOf) "+-<>.,"

parseExpr :: Parser [Bf]
parseExpr = many1 $ choice [try parseLoop, parseAtom]

-- Eval

act :: Op -> Act
act = \case
  '>'  -> Norm (\(Pointer p m) -> Pointer (p + 1) m)
  '<'  -> Norm (\(Pointer p m) -> Pointer (p - 1) m)
  '+'  -> Norm (\(Pointer p m) -> Pointer p (update p (ascend True) m))
  '-'  -> Norm (\(Pointer p m) -> Pointer p (update p (ascend False) m))
  '.'  -> Out (\(Pointer p m) -> flushStr [toEnum (m ! p)])
  ~',' -> In
    (\(Pointer p m) ->
      (\op -> Pointer p (update p (const $ fromEnum op) m)) <$> getChar
    )
 where
  update i op xs = V.update xs $ V.fromList [(i, op (xs ! i))]
  ascend True  i = i + 1 `mod` 256
  ascend False i = i - 1 `mod` 256

walk :: Pointer -> Bf -> IO Pointer
walk pt (Atom ops) = foldlM apply pt (map act ops)
 where
  apply x (Norm f) = return $ f x
  apply x (Out  f) = f x >> return x
  apply x (In   f) = f x
walk pt lp@(Loop lps) = if (mem pt ! pos pt) == 0
  then return pt
  else foldlM walk pt lps >>= flip walk lp
