{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Show.Internal where

import GHC.Generics
import Text.Show

appPrec :: Int
appPrec = 10

showsPrecDefault :: (Generic a, GShow (Rep a)) => Int -> a -> ShowS
showsPrecDefault d a = gshowsPrec (from a) d

class GShow f where
  gshowsPrec :: f p -> Int -> ShowS

instance (GShow f, GShow g) => GShow (f :+: g) where
  gshowsPrec (L1 f) = gshowsPrec f
  gshowsPrec (R1 g) = gshowsPrec g

instance GShow f => GShow (D1 c f) where
  gshowsPrec (M1 f) = gshowsPrec f

instance
  (GShowProduct f, Constructor ('MetaCons n x s))
  => GShow (C1 ('MetaCons n x s) f)
  where
  gshowsPrec c@(M1 f) = gshowsPrecProduct f (const (showString (conName c)))

class GShowProduct f where
  gshowsPrecProduct :: f p -> (Int -> ShowS) -> Int -> ShowS

instance GShowProduct U1 where
  gshowsPrecProduct U1 = id

instance (GShowProduct f, GShowProduct g) => GShowProduct (f :*: g) where
  gshowsPrecProduct (f :*: g) = gshowsPrecProduct g . gshowsPrecProduct f

instance Show a => GShowProduct (S1 c (K1 i a)) where
  gshowsPrecProduct (M1 (K1 a)) con d = showParen (d > appPrec) $
    con appPrec .
    showChar ' ' .
    showsPrec (appPrec + 1) a
