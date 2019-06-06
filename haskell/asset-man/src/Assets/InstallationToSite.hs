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


-- NOTE - with parametric context we can store increasing keys...

data Ctx0   = Ctx0
data Ctx1   = Ctx1 !String
data Ctx2   = Ctx2 !String !String
-- data Ctx3   = Ctx3 !String !String !String

type TransformE ctx a b = Transform ctx TranslateM a b
type RewriteE ctx a b = TransformE ctx a b

applyTransform :: RulesEnv -> TransformE Ctx0 a b -> a -> Either String b
applyTransform env t = runTranslateM env displayException . applyT t Ctx0

withContext :: c1 -> Transform c1 m a b -> Transform c m a b 
withContext c1 = liftContext (const c1)

installationToSite :: TransformE Ctx0 AibInstallation S4.S4Site
installationToSite = installation


installation :: TransformE Ctx0 AibInstallation S4.S4Site
installation = withPatFailExc (strategyFailure "installation") $ do 
    inst@AibInstallation {} <- idR
    info <- constT (getSiteFlocInfo (installation_ref inst))
    let instType = site_type info
    allfuns <- withContext (Ctx1 instType) $ 
                    aibInstallationT installationKid (\_ _ _ _ kids -> kids)
    return S4.S4Site 
              { S4.site_code           = site_level1_code info
              , S4.site_name           = site_s4_name info
              , S4.site_attributes     = installation_attributes inst
              , S4.site_kids           = S4.coalesceFunctions allfuns
              }

installationKid :: TransformE Ctx1 AibInstallationKid S4.S4Function
installationKid = 
    installationKid_ProcessGroup 
        <+ installationKid_Process 
        <+ catchall

installationKid_ProcessGroup :: TransformE Ctx1 AibInstallationKid S4.S4Function      
installationKid_ProcessGroup = withPatFailExc (strategyFailure "installationKid_ProcessGroup") $ do
    AibInstallationKid_ProcessGroup kid <- idR
    Ctx1 instType <- contextT 
    let groupName = process_group_name kid
    (funCode, _) <- constT $ getProcessGroupFlocInfo instType groupName
    procg <- aibInstallationKid_ProcessGroupT processGroup (\kids -> kids)
    constT $ makeS4Function funCode [procg]

installationKid_Process :: TransformE Ctx1 AibInstallationKid S4.S4Function  
installationKid_Process  = withPatFailExc (strategyFailure "installationKid_Process") $ do
    AibInstallationKid_Process {} <- idR
    Ctx1 instType   <- contextT
    let groupName = "NULL"
    (funCode, _) <- constT $ getProcessGroupFlocInfo instType groupName
    constT $ makeS4Function funCode []

catchall :: TransformE Ctx1 AibInstallationKid S4.S4Function  
catchall = constT $ makeS4Function "catchall" []

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

processGroup :: TransformE Ctx1 AibProcessGroup S4.S4ProcessGroup
processGroup = withPatFailExc (strategyFailure "processGroup") $ do
    group@AibProcessGroup {} <- idR
    Ctx1 instType <- contextT
    let groupName = process_group_name group
    (_, pgCode) <- return ("NULL", "NULL")
    pgDescr <- constT $ level3ProcessGroupDescription pgCode
    allprocs <- withContext (Ctx2 instType groupName) $ 
                    aibProcessGroupT processGroupKid (\_ _ _ kids -> kids)

    return $ S4.S4ProcessGroup 
                { S4.process_group_floc_code    = ""
                , S4.process_group_code         = pgCode
                , S4.process_group_name         = pgDescr
                , S4.process_group_attributes   = noAttrs
                , S4.process_group_kids         = allprocs
                }

processGroupKid :: TransformE Ctx2 AibProcessGroupKid S4.S4Process
processGroupKid = processGroupKid_Process
    
processGroupKid_Process :: TransformE Ctx2 AibProcessGroupKid S4.S4Process
processGroupKid_Process = 
    aibProcessGroupKid_ProcessT process (\a -> a)

process :: TransformE Ctx2 AibProcess S4.S4Process
process = withPatFailExc (strategyFailure "AibProcess") $ do
    proc@AibProcess {} <- idR
    Ctx2 siteType groupName <- contextT 
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

 