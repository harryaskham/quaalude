{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Quaalude.Tracers where

import System.IO.Unsafe (unsafePerformIO)

pauseId :: a -> a
pauseId a = unsafePerformIO $ do
  _ <- getLine
  return a

traceStrLn :: String -> a -> a
traceStrLn s a = unsafePerformIO $ do
  putStrLn s
  putStrLn ""
  return a

traceTextLn :: Text -> a -> a
traceTextLn s a = unsafePerformIO $ do
  putTextLn s
  putTextLn ""
  return a

traceWhen :: Bool -> (a -> a) -> a -> a
traceWhen True traceFn a = traceFn a
traceWhen False _ a = a

traceShowIdWhen :: Show a => (a -> Bool) -> a -> a
traceShowIdWhen p a
  | p a = traceShowId a
  | otherwise = a

traceUnless :: Bool -> (a -> a) -> a -> a
traceUnless True _ a = a
traceUnless False traceFn a = traceFn a

traceShowF :: Show b => (a -> b) -> a -> a
traceShowF f a = traceShow (f a) a

traceTextF :: (a -> Text) -> a -> a
traceTextF f a = traceTextLn (f a) a
