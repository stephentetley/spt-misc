{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.InstallationToSite
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Aib to S4 translation.
--
--------------------------------------------------------------------------------



module Assets.InstallationToSite 
    ( 
      TransformE
    , RewriteE
    , applyTransform
    , installationToSite
    ) where

import Control.Exception.Base   

import Language.KURE                    -- package: KURE

import Assets.TranslateMonad
import Assets.Common
import Assets.TranslationRules
import Assets.AibTypes
import Assets.AibUniverse
import qualified Assets.S4Types as S4




type TransformE a b = Transform () TranslateM a b
type RewriteE a b = TransformE a b

applyTransform :: RulesEnv -> TransformE a b -> a -> Either String b
applyTransform env t = runTranslateM env displayException . applyT t ()



installationToSite :: TransformE AibInstallation S4.S4Site
installationToSite = installation


installation :: TransformE AibInstallation S4.S4Site
installation = withPatFailExc (strategyFailure "installation") $ do 
    inst@AibInstallation {} <- idR
    info <- constT (getSiteFlocInfo (installation_ref inst))
    let instType = site_type info
    allfuns <- aibInstallationT (installationKid instType) (\_ _ _ _ kids -> kids)
    return S4.S4Site 
              { S4.site_code           = site_level1_code info
              , S4.site_name           = site_s4_name info
              , S4.site_attributes     = installation_attributes inst
              , S4.site_kids           = S4.coalesceFunctions allfuns
              }

installationKid :: String -> TransformE AibInstallationKid S4.S4Function
installationKid instType = 
    promoteT (installationKid_ProcessGroup instType)  <+ promoteT (installationKid_Process instType) 

installationKid_ProcessGroup :: String -> TransformE AibInstallationKid S4.S4Function      
installationKid_ProcessGroup instType = withPatFailExc (strategyFailure "installationKid_ProcessGroup") $ do
    AibInstallationKid_ProcessGroup kid <- idR
    let groupName = process_group_name kid
    (funCode, _) <- constT $ getProcessGroupFlocInfo instType groupName
    procg <- aibInstallationKid_ProcessGroupT (processGroup instType) (\kids -> kids)
    constT $ makeS4Function funCode [procg]

installationKid_Process :: String -> TransformE AibInstallationKid S4.S4Function  
installationKid_Process instType = withPatFailExc (strategyFailure "installationKid_Process") $ do
    AibInstallationKid_Process {} <- idR
    let groupName = "NULL"
    (funCode, _) <- constT $ getProcessGroupFlocInfo instType groupName
    constT $ makeS4Function funCode []

catchall :: String -> TransformE AibInstallationKid S4.S4Function  
catchall _ = constT $ makeS4Function "catchall" []

makeS4Function :: String -> [S4.S4ProcessGroup] -> TranslateM S4.S4Function
makeS4Function funCode procgs = do
    funName <- level2FunctionDescription funCode
    return $ S4.S4Function
        { S4.function_floc_code      = ""
        , S4.function_code           = funCode
        , S4.function_name           = funName
        , S4.function_attributes     = noAttrs
        , S4.function_kids           = procgs
        }

processGroup :: String -> TransformE AibProcessGroup S4.S4ProcessGroup
processGroup instType = withPatFailExc (strategyFailure "processGroup") $ do
    group@AibProcessGroup {} <- idR
    let groupName = process_group_name group
    (_, pgCode) <- return ("NULL", "NULL")
    pgDescr <- constT $ level3ProcessGroupDescription pgCode
    allprocs <- aibProcessGroupT (processGroupKid instType groupName) (\_ _ _ kids -> kids)

    return $ S4.S4ProcessGroup 
                { S4.process_group_floc_code    = ""
                , S4.process_group_code         = pgCode
                , S4.process_group_name         = pgDescr
                , S4.process_group_attributes   = noAttrs
                , S4.process_group_kids         = allprocs
                }

processGroupKid :: String -> String -> TransformE AibProcessGroupKid S4.S4Process
processGroupKid siteType groupName = processGroupKid_Process siteType groupName
    
processGroupKid_Process :: String -> String -> TransformE AibProcessGroupKid S4.S4Process
processGroupKid_Process siteType groupName = 
    aibProcessGroupKid_ProcessT (process siteType groupName) (\a -> a)

process :: String -> String -> TransformE AibProcess S4.S4Process
process siteType groupName = withPatFailExc (strategyFailure "AibProcess") $ do
    proc@AibProcess {} <- idR
    let procName = process_name proc
    (_, _, procCode) <- constT $ getProcessFlocInfo siteType groupName procName
    procDescr <- constT $ level4ProcessDescription procCode 
    return $ S4.S4Process
                { S4.process_floc_code          = ""
                , S4.process_code               = procCode
                , S4.process_name               = procDescr
                , S4.process_attributes         = noAttrs
                , S4.process_kids               = []
                }

 