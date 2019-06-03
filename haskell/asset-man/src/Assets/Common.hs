{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Asset.Common
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- S4 Types.
--
--------------------------------------------------------------------------------



module Assets.Common where

import Text.JSON                            -- package: json

import qualified Data.Map as Map

type Attributes = Map.Map String String



attributesToJSON :: Attributes -> JSValue
attributesToJSON = showJSON 