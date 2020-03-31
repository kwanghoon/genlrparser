{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Prim where

import Type
import Text.JSON.Generic

data PrimOp =
    NotPrimOp  --{l}. Bool -l-> Bool
  | OrPrimOp   --{l}. (Bool, Bool) -l-> Bool
  | AndPrimOp  --{l}. (Bool, Bool) -l-> Bool
  | EqPrimOp   --{l}. (Bool, Bool) -l-> Bool
  | NeqPrimOp  --{l}. (Bool, Bool) -l-> Bool
  | LtPrimOp   --{l}. (Int, Int) -l-> Bool
  | LePrimOp   --{l}. (Int, Int) -l-> Bool
  | GtPrimOp   --{l}. (Int, Int) -l-> Bool
  | GePrimOp   --{l}. (Int, Int) -l-> Bool
  | AddPrimOp  --{l}. (Int, Int) -l-> Int
  | SubPrimOp  --{l}. (Int, Int) -l-> Int
  | MulPrimOp  --{l}. (Int, Int) -l-> Int
  | DivPrimOp  --{l}. (Int, Int) -l-> Int
  | NegPrimOp  --{l}. Int -l-> Int
-- For aeson  
--  deriving (Show, Eq, Generic)
  deriving (Show, Eq, Typeable, Data)

primType tyname = ConType tyname [] []

bool_type = primType boolType
int_type  = primType intType
unit_type = primType unitType
string_type = primType stringType

primOpTypes :: [(PrimOp, ([Type], Type))]
primOpTypes =
  [ (NotPrimOp, ([bool_type], bool_type))
  , (OrPrimOp,  ([bool_type, bool_type], bool_type))
  , (AndPrimOp, ([bool_type, bool_type], bool_type))
  , (EqPrimOp,  ([bool_type, bool_type], bool_type))
  , (NeqPrimOp, ([bool_type, bool_type], bool_type))
  , (LtPrimOp,  ([int_type, int_type], bool_type))
  , (LePrimOp,  ([int_type, int_type], bool_type))
  , (GtPrimOp,  ([int_type, int_type], bool_type))
  , (GePrimOp,  ([int_type, int_type], bool_type))
  , (AddPrimOp, ([int_type, int_type], int_type))
  , (SubPrimOp, ([int_type, int_type], int_type))
  , (MulPrimOp, ([int_type, int_type], int_type))
  , (DivPrimOp, ([int_type, int_type], int_type))
  , (NegPrimOp, ([int_type], int_type))
  ]

lookupPrimOpType primop =
  [ (tys,ty) | (primop1,(tys,ty)) <- primOpTypes, primop==primop1]
