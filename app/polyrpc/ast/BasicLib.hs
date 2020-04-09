module BasicLib where

import Location
import Type
import Prim
import Expr


basicLib :: [(String, Type, Expr)]
basicLib =
  [

--   print : {l}. String -l->unit
--         = {l}. \x:String @l. primPrint [l] x

     let l = "l"
         x = "x"
     in 
     ( "print"
     , LocAbsType [l]
          (FunType string_type (LocVar l) unit_type)
     , LocAbs [l]
          (Abs [(x,string_type,LocVar l)]
               (Prim PrimPrintOp [LocVar l] [] [Var x]))
     ),
    

--   intToString
--     : {l}. Int -l-> String
--     = {l}. \x:Int @l. primIntToString [l] x
      let l = "l"
          x = "x"
      in
      ( "intToString"
      , LocAbsType [l]
          (FunType int_type (LocVar l) string_type)
      , LocAbs [l]
          (Abs [(x,int_type,LocVar l)]
            (Prim PrimIntToStringOp [LocVar l] [] [Var x]))
      ),

--   concat
--     : {l}. String -l-> String -l-> String
--     = {l}. \x:String @l  y:String @l. primConcat {l} (x,y)

      let l = "l"
          x = "x"
          y = "y"
      in
      ( "concat"
      , LocAbsType [l]
           (FunType string_type (LocVar l)
              (FunType string_type (LocVar l) string_type))
      , LocAbs [l]
           (Abs [(x,string_type,LocVar l)]
             (Abs [(y,string_type,LocVar l)]
                 (Prim PrimConcatOp [LocVar l] [] [Var x, Var y])))
      ),
    
  -- ("not", let l = "l" in
  --     LocAbsType [l] (FunType bool_type (LocVar l) bool_type)),


--    ref : {l1 l2}. [a]. a -l2-> Ref {l1} [a]
--        = {l1 l2}. [a].
--          \v : a @ l2. primRef {l1} [a] v

      let l1 = "l1"
          l2 = "l2"
          a  = "a"
          tyvar_a = TypeVarType a
          x  = "x"
      in
      ("ref"
      , LocAbsType [l1]
           (LocAbsType [l2]
              (TypeAbsType [a]
                  (FunType tyvar_a (LocVar l2)
                   (ConType refType [LocVar l1] [tyvar_a]))))
      , LocAbs [l1]
           (LocAbs [l2]
             (TypeAbs [a]
                 (Abs [(x,tyvar_a,LocVar l2)]
                    (Prim PrimRefCreateOp [LocVar l1] [tyvar_a] [Var x]))))
      ),


--   (!) : {l1 l2}. [a]. Ref {l1} [a] -l2-> a
--       = {l1 l2}. [a].
--         \addr:Ref {l1} [a] @l2. primRefRead {l1} [a] addr

     let l1 = "l1" 
         l2 = "l2" 
         a  = "a"
         tyvar_a = TypeVarType a
         x  = "x"
     in
     ( "!"
     , LocAbsType [l1]
          (LocAbsType [l2]
             (TypeAbsType [a]
                (FunType (ConType refType [LocVar l1] [tyvar_a])
                    (LocVar l2) tyvar_a)))
     , LocAbs [l1]
          (LocAbs [l2]
             (TypeAbs [a]
                 (Abs [(x,ConType refType [LocVar l1] [tyvar_a],LocVar l2)]
                      (Prim PrimRefReadOp [LocVar l1] [tyvar_a] [Var x]))))
     ),


--  (:=) : {l1 l2}. [a]. Ref {l1} [a] -l2-> a -l2-> Unit
--       = {l1 l2}. [a].
--         \addr: Ref {l1} [a] @l2. newv: a @l2. primWrite {l1} [a] addr newv


     let l1 = "l1" 
         l2 = "l2" 
         a  = "a"
         tyvar_a = TypeVarType a
         x  = "x"
         y  = "y"
     in
     ( ":="
     , LocAbsType [l1]
          (LocAbsType [l2]
              (TypeAbsType [a]
                  (FunType
                      (ConType refType [LocVar l1] [tyvar_a])
                      (LocVar l2)
                      (FunType tyvar_a (LocVar l2) unit_type))))
     , LocAbs [l1]
          (LocAbs [l2]
              (TypeAbs [a]
                  (Abs [(x,ConType refType [LocVar l1] [tyvar_a],LocVar l2)]
                      (Abs [(y,tyvar_a,LocVar l2)]
                          (Prim PrimRefWriteOp [LocVar l1] [tyvar_a] [Var x, Var y])))))
     )
  ]
