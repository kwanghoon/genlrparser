module SynCompInterface where

data EmacsDataItem =
    LexError
  | SuccessfullyParsed
  | Candidate String
  deriving Show
