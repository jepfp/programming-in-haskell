-- main program
-- compile with ghc -O2 hangman.hs
-- run with ./hangman

import System.IO (hSetEcho, stdin, stdout, hSetBuffering, BufferMode (NoBuffering))

main :: IO ()
main = do
  -- Disable buffering in order to see the characters immediately on the terminal
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then
      putStrLn "You got it!"
    else do
      putStrLn (match word guess)
      play word

match :: String -> String -> String
match [] ys = ""
match (x : xs) ys =
  if x `elem` ys
    then
      x : match xs ys
    else
      '-' : match xs ys
