{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE InstanceSigs               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.FlocPath
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- FlocPath
--
--------------------------------------------------------------------------------


module Assets.FlocPath 
    (
      FlocPath  
    , singleton
    , snoc
    , toString
    ) where

import Language.KURE.Path               -- package: kure

import Data.List

newtype FlocPath = FlocPath { getFlocPath :: SnocPath String }
    deriving (Eq, Show)

singleton :: String -> FlocPath 
singleton s = FlocPath $ singletonSnocPath s

snoc :: String -> FlocPath -> FlocPath
snoc s path = FlocPath $ getFlocPath path @@ s

toString :: FlocPath -> String
toString (FlocPath ss) = 
    concat $ intersperse "-" $ snocPathToPath ss 


instance ReadPath FlocPath String where      
    absPath :: FlocPath -> AbsolutePath String
    absPath (FlocPath ss) = ss 


instance ExtendPath  FlocPath String where      
    (FlocPath ss) @@ s1 = FlocPath $ ss @@ s1
