{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Location where

import Text.JSON.Generic

data Location =
    Location String
  | LocVar String
-- For aeson  
--  deriving (Show, Generic)
  deriving (Show, Typeable, Data)

equalLoc (Location x) (Location y) = x==y
equalLoc (LocVar x) (LocVar y) = x==y
equalLoc _ _ = False
