{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Examples where

import Generic.Show
import Reflection
import Data.Functor.Classes (Show1, showsPrec1)

data T f a = T (f a) a
  deriving Generic

showsT :: forall f a. (Show (f a), Show a) => Int -> T f a -> ShowS
showsT = showsPrecDefault

showsT1 :: forall f a. (Show1 f, Show a) => Int -> T f a -> ShowS
showsT1 = using (RShow (showsPrec1 @f @a)) showsT
