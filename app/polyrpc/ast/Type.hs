{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Type where

import Data.Char
-- For aeson
-- import GHC.Generics
-- import Data.Aeson
import Text.JSON.Generic

import Location

data Type =
    TypeVarType String
  | TupleType [Type]
  | FunType Type Location Type
  | TypeAbsType [String] Type
  | LocAbsType [String] Type
  | ConType String [Type]
  | RefType Location Type
-- For aeson  
--  deriving (Show, Generic)
  deriving (Show, Typeable, Data)

singleTypeAbsType (TypeAbsType [] expr) = expr
singleTypeAbsType (TypeAbsType [a] expr) = TypeAbsType [a] expr
singleTypeAbsType (TypeAbsType (a:as) expr) = TypeAbsType [a] (singleTypeAbsType (TypeAbsType as expr))
singleTypeAbsType other = other

singleLocAbsType (LocAbsType [] expr) = expr
singleLocAbsType (LocAbsType [a] expr) = LocAbsType [a] expr
singleLocAbsType (LocAbsType (a:as) expr) = LocAbsType [a] (singleLocAbsType (LocAbsType as expr))
singleLocAbsType other = other


--
-- For aeson
-- instance ToJSON Location where
-- instance ToJSON Type where

-- Names
isTypeName (c:s) = isUpper c
isTypeName _     = False

isTypeVarName (c:s) = isLower c
isTypeVarName _ = False

isBindingName (c:s) = isLower c
isBindingName _     = False

isConstructorName (c:s) = isUpper c
isConstructorName _     = False


-- Predefined type names
unitType   = "Unit"
intType    = "Int"
boolType   = "Bool"
stringType = "String"

-- Predefined location names
clientLoc = Location "client"
serverLoc = Location "server"


--
doSubstOne :: String -> Type -> Type -> Type
doSubstOne x ty (TypeVarType y)
  | x==y = ty
  | otherwise = (TypeVarType y)
doSubstOne x ty (TupleType tys) =
  TupleType (map (doSubstOne x ty) tys)
doSubstOne x ty (FunType argty loc retty) =
  FunType (doSubstOne x ty argty) loc (doSubstOne x ty retty)
doSubstOne x ty (TypeAbsType tyvars bodyty)
  | elem x tyvars = (TypeAbsType tyvars bodyty)
  | otherwise = (TypeAbsType tyvars (doSubstOne x ty bodyty))
doSubstOne x ty (LocAbsType tyvars bodyty) =
  LocAbsType tyvars (doSubstOne x ty bodyty)
doSubstOne x ty (ConType name tys) =
  ConType name (map (doSubstOne x ty) tys)
doSubstOne x ty (RefType loc valty) =
  RefType loc (doSubstOne x ty valty)

doSubst :: [(String,Type)] -> Type -> Type
doSubst [] ty0 = ty0
doSubst ((x,ty):subst) ty0 = 
  doSubst subst (doSubstOne x ty ty0)

--
doSubstLocOverLoc :: String -> Location -> Location -> Location
doSubstLocOverLoc x loc (Location name) = Location name
doSubstLocOverLoc x loc (LocVar y)
  | x == y = loc
  | otherwise = LocVar y

doSubstLocOne :: String -> Location -> Type -> Type
doSubstLocOne x loc (TypeVarType y) = (TypeVarType y)
doSubstLocOne x loc (TupleType tys) =
  TupleType (map (doSubstLocOne x loc) tys)
doSubstLocOne x loc (FunType argty loc0 retty) =
  FunType (doSubstLocOne x loc argty)
    (doSubstLocOverLoc x loc loc0) (doSubstLocOne x loc retty)
doSubstLocOne x loc (TypeAbsType tyvars bodyty) =
  TypeAbsType tyvars (doSubstLocOne x loc bodyty)
doSubstLocOne x loc (LocAbsType locvars bodyty)
  | elem x locvars = LocAbsType locvars bodyty
  | otherwise = LocAbsType locvars (doSubstLocOne x loc bodyty)
doSubstLocOne x loc (ConType name tys) =
  ConType name (map (doSubstLocOne x loc) tys)
doSubstLocOne x loc (RefType loc0 valty) =
  RefType (doSubstLocOverLoc x loc loc0) (doSubstLocOne x loc valty)


doSubstLoc :: [(String, Location)] -> Type -> Type
doSubstLoc [] ty = ty
doSubstLoc ((x,loc):substLoc) ty =
  doSubstLoc substLoc (doSubstLocOne x loc ty)

--
equalType :: Type -> Type -> Bool
equalType ty1 ty2 = equalTypeWithFreshness [1..] ty1 ty2

equalTypeWithFreshness ns (TypeVarType x) (TypeVarType y) = x==y

equalTypeWithFreshness ns (TupleType tys1) (TupleType tys2) =
  and (map (uncurry (equalTypeWithFreshness ns)) (zip tys1 tys2))
  
equalTypeWithFreshness ns (FunType argty1 loc1 retty1) (FunType argty2 loc2 retty2) =
  equalTypeWithFreshness ns argty1 argty2 && equalLoc loc1 loc2 && equalTypeWithFreshness ns retty1 retty2
  
equalTypeWithFreshness ns (TypeAbsType tyvars1 ty1) (TypeAbsType tyvars2 ty2) =
  let len1 = length tyvars1
      len2 = length tyvars2
      newvars = map (TypeVarType . show) (take len1 ns)
      ns'     = drop len1 ns
  in len1==len2 && equalTypeWithFreshness ns' (doSubst (zip tyvars1 newvars) ty1) (doSubst (zip tyvars2 newvars) ty2)
     
equalTypeWithFreshness ns (LocAbsType locvars1 ty1) (LocAbsType locvars2 ty2) =
  let len1 = length locvars1
      len2 = length locvars2
      newvars = map (LocVar . show) (take len1 ns)
      ns'     = drop len1 ns
  in len1==len2 && equalTypeWithFreshness ns' (doSubstLoc (zip locvars1 newvars) ty1) (doSubstLoc (zip locvars2 newvars) ty2)

equalTypeWithFreshness ns (ConType name1 tys1) (ConType name2 tys2) =   
  name1==name2 && and (map (uncurry (equalTypeWithFreshness ns)) (zip tys1 tys2))

equalTypeWithFreshness ns (RefType loc1 ty1) (RefType loc2 ty2) =
  equalLoc loc1 loc2 && equalTypeWithFreshness ns ty1 ty2

--
occur :: String -> Type -> Bool
occur x (TypeVarType y) = x==y
occur x (TupleType tys) = and (map (occur x) tys)
occur x (FunType argty loc retty) = occur x argty && occur x retty
occur x (ConType c tys) = and (map (occur x) tys)
occur x (RefType loc ty) = occur x ty
occur x (TypeAbsType _ _) = False  -- ???
occur x (LocAbsType _ _) = False -- ???

unifyTypeOne :: Type -> Type -> Maybe [(String,Type)]
unifyTypeOne (TypeVarType x) (TypeVarType y)
  | x==y = Just []
  | otherwise = Just [(x, TypeVarType y)]
  
unifyTypeOne (TypeVarType x) ty
  | occur x ty = Nothing
  | otherwise = Just [(x,ty)]

unifyTypeOne ty (TypeVarType x)
  | occur x ty = Nothing
  | otherwise = Just [(x,ty)]

unifyTypeOne (TupleType tys1) (TupleType tys2) = unifyTypes tys1 tys2

unifyTypeOne (FunType argty1 loc1 retty1) (FunType argty2 loc2 retty2) =  -- loc1 and loc2 ??
  case unifyTypeOne argty1 argty2 of
    Nothing -> Nothing
    Just subst1 ->
      case unifyTypeOne (doSubst subst1 retty1) (doSubst subst1 retty2) of
        Nothing -> Nothing
        Just subst2 -> Just (subst1 ++ subst2)

unifyTypeOne (ConType c1 tys1) (ConType c2 tys2)
  | c1==c2 = unifyTypes tys1 tys2
  | otherwise = Nothing

unifyTypeOne (RefType loc1 ty1) (RefType loc2 ty2) = unifyTypeOne ty1 ty2 -- loc1 and loc2 ??

unifyTypeOne _ _ = Nothing   -- universal types and locations ???

unifyTypes :: [Type] -> [Type] -> Maybe [(String,Type)]
unifyTypes [] [] = Just []
unifyTypes (ty1:tys1) (ty2:tys2) =
  case unifyTypeOne ty1 ty2 of
    Nothing -> Nothing
    Just subst1 ->
      case unifyTypes (map (doSubst subst1) tys1) (map (doSubst subst1) tys2) of
        Nothing -> Nothing
        Just subst2 -> Just (subst1 ++ subst2)
        
