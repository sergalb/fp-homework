module Task6
  ( distributivityWhnf
  , secondWhnf
  ) where

import Prelude hiding (null)

-- | expresion in whnf
distributivityWhnf :: (Either String b, Either String c)
distributivityWhnf = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | expression in whnf. 
-- arg of null looks like [23.140692632779267 : thunk]
-- it enough to get answer 
secondWhnf :: Bool
secondWhnf = False
