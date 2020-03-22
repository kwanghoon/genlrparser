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

  -- ref :: {l1 l2}. [a]. a -l2-> Ref l1 a
  
  ("ref", let l1 = "l1" in
      let l2 = "l2" in
      let a = "a" in
        let tyvar_a = TypeVarType a in
        singleLocAbsType (LocAbsType [l1, l2]
        (TypeAbsType [a]
         (FunType tyvar_a (LocVar l2) (RefType (LocVar l1) tyvar_a))))),

-- (!) :: {l1 l2}. [a]. Ref l1 a -l2-> a
  
  ("!", let l1 = "l1" in
      let l2 = "l2" in
        let a = "a" in
        let tyvar_a = TypeVarType a in
          singleLocAbsType (LocAbsType [l1,l2]
          (TypeAbsType [a]
          (FunType (RefType (LocVar l1) tyvar_a) (LocVar l2) tyvar_a)))),

-- (:=) :: {l1 l2}. [a]. Ref l1 a -l2-> a -l2-> Unit
  
  (":=", let l1 = "l1" in
      let l2 = "l2" in
        let a = "a" in
          let tyvar_a = TypeVarType a in
            singleLocAbsType (LocAbsType [l1, l2]
            (TypeAbsType [a]
            (FunType (RefType (LocVar l1) tyvar_a) (LocVar l2)
             (FunType tyvar_a (LocVar l2) unit_type)))) )
  ]
