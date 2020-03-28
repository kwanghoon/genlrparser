{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Location where

import Text.JSON.Generic

data Location =
    Location String
  | LocVar LocationVar
  deriving (Show, Typeable, Data)

equalLoc (Location x) (Location y) = x==y
equalLoc (LocVar x) (LocVar y) = x==y
equalLoc _ _ = False

equalLocs [] [] = True
equalLocs (l1:locs1) (l2:locs2) = equalLoc l1 l2 && equalLocs locs1 locs2
equalLocs _ _ = False

type LocationVar = String