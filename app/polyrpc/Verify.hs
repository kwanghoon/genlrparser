module Verify where

import Location
import CSType
import CSExpr


verify :: Monad m => GlobalTypeInfo -> FunctionStore -> [TopLevelDecl] -> m ()
verify gti funStore toplevelDecls = do
  



