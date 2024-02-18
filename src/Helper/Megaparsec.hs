{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Helper.Megaparsec where

import Helper.Util
import Replace.Megaparsec
import Text.Megaparsec

type MParser a = Parsec Void String a

parserM :: (Stream s, Ord e) => Parsec e s a -> s -> a
parserM p t = unjust $ parseMaybe p t

parseMatches :: MParser a -> String -> [a]
parseMatches p = rights . parserM (sepCap p)
