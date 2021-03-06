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

import Control.Exception    
import qualified Data.Map as Map
-- import Data.List

import Text.JSON                        -- package: json
import Language.KURE                    -- package: KURE



import Assets.Common
import Assets.FlocLabelling
import Assets.FlocPath
import Assets.AibTypes
import Assets.AibPretty
import Assets.S4Types
-- import Assets.TranslationRules
import Assets.TranslateMonad
import Assets.InstallationToSite
import qualified Assets.S4Pretty as S4


attrs :: Attributes
attrs = Map.fromList [("gridRef", AttrString "SE7865490277")]

demo0a :: Result Attributes
demo0a = attributesFromJSON $ attributesToJSON attrs


demo0b :: String
demo0b = toString $ singleton "ABB01" @@ "CAA" @@ "TEL"


rulesConfig :: RulesConfig
rulesConfig = RulesConfig
    { path_to_levels_1_2_mapping_file       = "rules/aib_to_s4_level1_2.json"
    , path_to_levels_2_3_mapping_file       = "rules/aib_to_s4_level2_3.json"
    , path_to_levels_2_3_4_mapping_file     = "rules/aib_to_s4_level2_3_4.json"
    , path_to_level_2_descriptions          = "rules/level2_descriptions.json"
    , path_to_level_3_descriptions          = "rules/level3_descriptions.json"
    , path_to_level_4_descriptions          = "rules/level4_descriptions.json"
    }




demo01 :: IO ()
demo01 = do
    json <- readFile "data/aib_ald.json"
    env <- loadRules $ rulesConfig
    let (aibResult::Result AibInstallation) = decodeStrict json
    case aibResult of
        Error errMsg -> error errMsg
        Ok a1 -> do
            writeFile "demo/output/aib_output_ald.txt" (aibDrawTree a1)
            case applyTransform env installationToSite a1 of
                Left errMsg -> error errMsg
                Right ans -> do
                    let output = encode ans 
                    writeFile "demo/output/s4_output_ald.json" output
                    writeFile "demo/output/s4_output_ald.txt" (S4.s4DrawTree ans)



demo02 :: IO S4Site
demo02 = do
    json <- readFile "data/s4_ald.json"
    let (ans::Result S4Site) = decodeStrict json
    case ans of
        Error errMsg -> error errMsg
        Ok a1 -> return a1    
        

testFloc2 :: String -> IO ()
testFloc2 procGroup = do
    env <- loadRules $ rulesConfig
    print procGroup
    print $ runTranslateM env displayException $ do
        getProcessGroupFlocInfo "STW" procGroup

demo03 :: IO ()
demo03 = 
    mapM_ testFloc2 $
        [ "CHEMICAL SERVICES"
        , "CONTROL SERVICES"
        , "ELECTRICAL SERVICES"
        , "INSTALLATION SERVICES"
        , "PRELIMINARY TREATMENT-STW"
        , "PRIMARY TREATMENT"
        , "SECONDARY TREATMENT"
        , "SLUDGE TREATMENT"
        , "STATUTORY"
        , "STORM TREATMENT"
        , "TERTIARY TREATMENT"
        ]

