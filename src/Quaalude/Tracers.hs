{-# OPTIONS_GHC -Wno-deprecations #-}

module Quaalude.Tracers where

import Control.Concurrent
import Data.Text qualified as T
import Debug.Trace qualified as Trace
import System.Console.ANSI
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Text.ParserCombinators.Parsec

pauseId :: a -> a
pauseId a = unsafePerformIO $ do
  _ <- getLine
  return a

traceStrLn :: String -> a -> a
traceStrLn s a = unsafePerformIO $ do
  putStrLn s
  putStrLn ""
  return a

traceStrLnId :: (Show a) => a -> a
traceStrLnId a = unsafePerformIO $ do
  putStrLn (show a)
  putStrLn ""
  return a

traceTextLn :: Text -> a -> a
traceTextLn s a = unsafePerformIO $ do
  putTextLn s
  putTextLn ""
  return a

traceTextLnId :: (Show a) => a -> a
traceTextLnId a = unsafePerformIO $ do
  putTextLn (show a)
  putTextLn ""
  return a

traceWhen :: Bool -> (a -> a) -> a -> a
traceWhen True traceFn a = traceFn a
traceWhen False _ a = a

traceShowIdWhen :: (Show a) => (a -> Bool) -> a -> a
traceShowIdWhen p a
  | p a = traceShowId a
  | otherwise = a

traceUnless :: Bool -> (a -> a) -> a -> a
traceUnless True _ a = a
traceUnless False traceFn a = traceFn a

traceShowF :: (Show b) => (a -> b) -> a -> a
traceShowF f a = traceShow (f a) a

traceTextF :: (a -> Text) -> a -> a
traceTextF f a = traceTextLn (f a) a

ptrace' :: Bool -> String -> Parser a -> Parser a
ptrace' True = parserTraced
ptrace' False = const id

ptrace :: Parser a -> Parser a
ptrace = ptrace' True "ptrace"

traceAnim :: (Show a) => Double -> a -> b -> b
traceAnim fps a b = unsafePerformIO do
  let uspf = 1000000.0 / fps
  clearScreen
  print a
  threadDelay (round uspf)
  return b

traceStack :: Text -> a -> a
traceStack msg a = Trace.traceStack (T.unpack msg) a

traceStackId :: a -> a
traceStackId = traceStack ""

traceIf :: Bool -> String -> a -> a
traceIf b s = if b then traceShow s else id

tracePrefixId :: (Show a, Show b) => a -> b -> b
tracePrefixId a b = traceShow (a, b) b

tracePrefixIdM :: (Monad m, Show a, Show b) => a -> m b -> m b
tracePrefixIdM a mb =
  traceShow a $ do
    b <- mb
    traceShow b (pure b)

traceN f xs a = foldl' (flip f) a xs

traceNId f xs = traceN f xs xs
