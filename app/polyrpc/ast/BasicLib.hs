module BasicLib where

import Location
import Type
import Prim
import Expr


basicLib :: [(String, Type)]
basicLib =
  [
{-
   print : {l}. String -l->unit
         = {l}. \x:String @l. primPrint [l] x
-}
  ("print", let l = "l" in
      LocAbsType [l] (FunType string_type (LocVar l) unit_type)),
    
{-
   intToString
     : {l}. Int -l-> String
     = {l}. \x:Int @l. primIntToString [l] x
-}
  
  ("intToString", let l = "l" in
      LocAbsType [l] (FunType int_type (LocVar l) string_type)),

{-
   concat
     : {l}. String -l-> String -l-> String
     = {l}. \x:String @l  y:String @l. primConcat {l} (x,y)
-}
  
  ("concat", let l = "l" in
      LocAbsType [l] (FunType string_type (LocVar l)
                      (FunType string_type (LocVar l) string_type))),
    
  -- ("not", let l = "l" in
  --     LocAbsType [l] (FunType bool_type (LocVar l) bool_type)),

{-
    ref : {l1 l2}. [a]. a -l2-> Ref {l1} [a]
        = {l1 l2}. [a].
          \v : a @ l2. primRef {l1} [a] v
-}
  
  ("ref", let l1 = "l1" in
      let l2 = "l2" in
      let a = "a" in
        let tyvar_a = TypeVarType a in
        singleLocAbsType (LocAbsType [l1, l2]
        (TypeAbsType [a]
         (FunType tyvar_a (LocVar l2) (ConType refType [LocVar l1] [tyvar_a]))))),

{-
   (!) : {l1 l2}. [a]. Ref {l1} [a] -l2-> a
       = {l1 l2}. [a].
         \addr:Ref {l1} [a] @l2. primRefRead {l1} [a] addr
-}
  
  ("!", let l1 = "l1" in
      let l2 = "l2" in
        let a = "a" in
        let tyvar_a = TypeVarType a in
          singleLocAbsType (LocAbsType [l1,l2]
          (TypeAbsType [a]
          (FunType (ConType refType [LocVar l1] [tyvar_a]) (LocVar l2) tyvar_a)))),

{-
  (:=) : {l1 l2}. [a]. Ref {l1} [a] -l2-> a -l2-> Unit
       = {l1 l2}. [a].
         \addr: Ref {l1} [a] @l2. newv: a @l2. primWrite {l1} [a] addr newv
-}
  
  (":=", let l1 = "l1" in
      let l2 = "l2" in
        let a = "a" in
          let tyvar_a = TypeVarType a in
            singleLocAbsType (LocAbsType [l1, l2]
            (TypeAbsType [a]
            (FunType (ConType refType [LocVar l1] [tyvar_a]) (LocVar l2)
             (FunType tyvar_a (LocVar l2) unit_type)))) )
  ]
