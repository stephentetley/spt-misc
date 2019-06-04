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

import Text.JSON                        -- package: json
import Language.KURE                    -- package: KURE

import qualified Data.Map as Map
-- import Data.List

import Assets.Facts.SiteNameMapping
import Assets.Common
import Assets.FlocPath
import Assets.AibTypes
import Assets.S4Types
import Assets.InstallationToSite


attrs :: Attributes
attrs = Map.fromList [("gridRef", AttrString "SE7865490277")]

demo0a :: Result Attributes
demo0a = attributesFromJSON $ attributesToJSON attrs


demo0b :: String
demo0b = toString $ singleton "ABB01" @@ "CAA" @@ "TEL"


demo0c :: Maybe SiteDetails
demo0c = siteNameMapping "SAI00001000"




demo01 :: IO ()
demo01 = do
    json <- readFile "data/aib_ald.json"
    let (aibResult::Result AibInstallation) = decodeStrict json
    case aibResult of
        Error errMsg -> error errMsg
        Ok a1 -> 
            case applyTransform installationToSite a1 of
                Left errMsg -> error errMsg
                Right ans -> 
                    let output = encode ans 
                    in writeFile "demo/output/s4_output_ald.json" output



demo02 :: IO S4Site
demo02 = do
    json <- readFile "data/s4_ald.json"
    let (ans::Result S4Site) = decodeStrict json
    case ans of
        Error errMsg -> error errMsg
        Ok a1 -> return a1                    

