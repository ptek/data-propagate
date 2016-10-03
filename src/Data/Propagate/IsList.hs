{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Propagate.IsList where

import           Data.Propagate (PtEvent, PtQueue)
import qualified Data.Propagate as P
import           GHC.Exts

-- | Useful with OverloadedLists Extension
--
instance IsList (PtQueue a) where
  type Item (PtQueue a) = PtEvent a
  fromList = P.fromList
  toList   = P.toList
