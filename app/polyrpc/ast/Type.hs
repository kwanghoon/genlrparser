module Type where

data Location =
      Location String
    | LocVar String

data Type =
    TypeVarType String
  | TupleType [Type]
  | FunType Type Location Type
  | TypeAbsType [String] Type
  | LocAbsType [String] Type
  | ConType String [Type]

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

data Location = Location String

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
  | otherwise = (TypeAbsType tyvars (doSubsOne x ty bodyty))
doSubstOne x ty (LocAbsType tyvars bodyty) =
  LocAbsType tyvars (doSubstOne x ty bodyty)
doSubstOne x ty (ConType name tys) =
  ConType name (map (doSubstOne x ty) tys)

doSubst :: [(String,Type)] -> Type -> Type
doSubst [] ty0 = ty0
doSubst ((x,ty):subst) ty0 = ty0
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
  FunType (doSubstLocOne x ty argty)
    (doSubstLocOverLoc x loc loc0) (doSubstLocOne x ty retty)
doSubstLocOne x loc (TypeAbsType tyvars bodyty) =
  TypeAbsType tyvars (doSubsLocOne x loc bodyty)
doSubstLocOne x loc (LocAbsType locvars bodyty)
  | elem x locvars = LocAbsType locvars bodyty
  | otherwise = LocAbsType locvars (doSubstLocOne x loc bodyty)
doSubstLocOne x loc (ConType name tys) =
  ConType name (map (doSubstLocOne x loc) tys)


doSubstLoc :: [(String, Location)] -> Type -> Type
doSubstLoc [] ty = ty
doSubstLoc ((x,loc):substLoc) ty =
  doSubstLoc substLoc (doSubstLocOne x loc ty)

--
equalType :: Type -> Type -> Bool
equalType ty1 ty2 = equalTypeWithFreshness [1..] ty1 ty2

equalTypeWithFreshness ns (TypeVarType x) (TypeVarType y) = x==y

equalTypeWithFreshness ns (TupleType tys1) (TypleType tys2) =
  and (map (uncurry (equalTypeWithFreshness ns)) (zip tys1 tys2)
  
equalTypeWithFreshness ns (FunType argty1 loc1 retty1) (FunType argty2 loc2 retty2) =
  equalTypeWithFreshness ns argty1 argty2 && equalLoc loc1 loc2 && equalTypeWithFreshness ns retty1 retty2
  
equalTypeWithFreshness ns (TypeAbsType tyvars1 ty1) (TypeAbsType tyvars2 ty2) =
  let len1 = length tyvars1
      len2 = length tyvars2
      newvars = map show (take len1 ns)
      ns'     = drop len1 ns
  in len1==len2 && equalWithFreshness ns' (doSubst (tyvars1 newvars) ty1) (doSubst (tyvars2 newvars) ty2)
     
equalTypeWithFreshness ns (LocAbsType locvars1 ty1) (LocAbsType locvars2 ty2) =
  let len1 = length locvars1
      len2 = length locvars2
      newvars = map show (take len1 ns)
      ns'     = drop len1 ns
  in len1==len2 && equalWithFreshness ns' (doSubstLoc (locvars1 newvars) ty1) (doSubstLoc (locvars2 newvars) ty2)

equalTypeWithFreshness ns (ConType name1 tys1) (ConType name2 tys2) =   
  name1==name2 && and (map (uncurry (equalTypeWithFreshness ns) (zip tys1 tys2)

--
unifyType :: Type -> Type -> Maybe [(String,Type)]


unifTypes :: [Type] -> [Type] -> Maybe [(String,Type)]


