{-# LANGUAGE DeriveDataTypeable #-}
module Records where

import Data.Data

data TopLevel = TopLevel {
	thing :: Int,
	subs :: [SubLevel]
} deriving (Eq, Data, Typeable)

data SubLevel = SubLevel {
	thing2 :: Bool,
	other :: Maybe String
} deriving (Eq, Data, Typeable)
