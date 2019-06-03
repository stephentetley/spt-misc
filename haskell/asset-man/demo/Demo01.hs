{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Demo01
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


module Demo01 where

import Text.JSON

import qualified Data.Map as Map

import Assets.Common
import Assets.AibTypes
import Assets.S4Types
import Assets.InstallationToSite


attrs :: Attributes
attrs = Map.fromList [("gridRef", AttrString "SE7865490277")]

demo01 :: Result Attributes
demo01 = attributesFromJSON $ attributesToJSON attrs

demo02 :: IO S4Site
demo02 = do
    json <- readFile "data/s4_ald.json"
    let (ans::Result S4Site) = decodeStrict json
    case ans of
        Error errMsg -> error errMsg
        Ok a1 -> return a1


demo03 :: IO AibInstallation
demo03 = do
    json <- readFile "data/aib_ald.json"
    let (ans::Result AibInstallation) = decodeStrict json
    case ans of
        Error errMsg -> error errMsg
        Ok a1 -> return a1