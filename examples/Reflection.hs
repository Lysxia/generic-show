{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflection where

import Data.Coerce
import Data.Proxy
import Data.Reflection
import Text.Show
import Unsafe.Coerce

data RShow a = RShow {
  rshowsPrec :: Int -> a -> ShowS }

newtype RefShow s a = RefShow a

instance Reifies s (RShow a) => Show (RefShow s a) where
  showsPrec = coerce (rshowsPrec (reflect (Proxy :: Proxy s)))

newtype Magic c a = Magic (c => a)

using :: forall a t. RShow a -> (Show a => t) -> t
using a f =
  let g :: forall s. Reifies s (RShow a) => t
      g =
        let Magic g' = (unsafeCoerce :: Magic (Show a) t -> Magic (Show (RefShow s a)) t)
              (Magic f)
        in g'
  in reify a (\(_ :: Proxy s) -> g @s)

