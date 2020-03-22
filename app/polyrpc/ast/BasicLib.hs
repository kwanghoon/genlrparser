module BasicLib where

import Location
import Type
import Expr


basicLib :: [(String, Type)]
basicLib =
  [
  ("print", let l = "l" in
      LocAbsType [l] (FunType string_type (LocVar l) unit_type)),
  ("not", let l = "l" in
      LocAbsType [l] (FunType bool_type (LocVar l) bool_type)),
  ("ref", let l = "l" in
      let a = "a" in
        let tyvar_a = TypeVarType a in
        LocAbsType [l]
        (TypeAbsType [a]
         (FunType tyvar_a (LocVar l) (RefType (LocVar l) tyvar_a)))),
  ("!", let l1 = "l1" in
      let l2 = "l2" in
        let a = "a" in
        let tyvar_a = TypeVarType a in
          singleLocAbsType (LocAbsType [l1,l2]
          (TypeAbsType [a]
          (FunType (RefType (LocVar l1) tyvar_a) (LocVar l2) tyvar_a)))),
  (":=", let l1 = "l1" in
      let l2 = "l2" in
        let a = "a" in
          let tyvar_a = TypeVarType a in
            singleLocAbsType (LocAbsType [l1, l2]
            (TypeAbsType [a]
            (FunType (RefType (LocVar l1) tyvar_a) (LocVar l2)
             (FunType tyvar_a (LocVar l2) unit_type)))) )
  ]
