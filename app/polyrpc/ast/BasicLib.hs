module BasicLib where

import Type
import Expr


basicLib :: [(String, Type)]
basicLib = [
  ("print", let l = "l" in
      LocAbsType [l] (FunType string_type (LocVar l) unit_type)) ]
