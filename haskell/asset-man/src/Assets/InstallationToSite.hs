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
import Assets.Facts.CodeMapping   
import Assets.Facts.CodeNames
import Assets.Facts.SiteNameMapping
import Assets.Common
import Assets.AibTypes
import Assets.AibUniverse
import qualified Assets.S4Types as S4


data Ctx 
    = CtxNone
    | CtxOne    String                    -- Site
    | CtxTwo    String  String            -- Site x ProcGroup
    | CtxThree  String  String  String    -- Site x ProcGroup x Proc



push :: String -> Ctx -> Ctx
push item (CtxNone)             = CtxOne item
push item (CtxOne site)         = CtxTwo site item
push item (CtxTwo site pgrp)    = CtxThree site pgrp item
push _    full                  = full





type TransformE a b = Transform Ctx (TranslateM ()) a b
type RewriteE a b = TransformE a b

applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runTranslateM () displayException . applyT t CtxNone



installationToSite :: TransformE AibInstallation S4.S4Site
installationToSite = installation


installation :: TransformE AibInstallation S4.S4Site
installation = do 
    inst@AibInstallation {} <- idR
    info <- siteNameMapping (installation_ref inst)
    let siteType = site_type info
    allfuns <- liftContext (push siteType) $ aibInstallationT installationKid (\_ _ _ _ kids -> kids)
    return S4.S4Site 
              { S4.site_code           = floc1 info
              , S4.site_name           = site_name info
              , S4.site_attributes     = installation_attributes inst
              , S4.site_kids           = S4.coalesceFunctions allfuns
              }

installationKid :: TransformE AibInstallationKid S4.S4Function
installationKid = installationKid_ProcessGroup <+ installationKid_Process

installationKid_ProcessGroup :: TransformE AibInstallationKid S4.S4Function      
installationKid_ProcessGroup = withPatFailExc (strategyFailure "ProcessGroup") $ do
    AibInstallationKid_ProcessGroup kid <- idR
    CtxOne siteType <- contextT 
    let groupName = process_group_name kid
    (funCode, _) <- codeMapping2 (siteType, groupName)
    makeS4Function funCode

installationKid_Process :: TransformE AibInstallationKid S4.S4Function  
installationKid_Process = withPatFailExc (strategyFailure "Process") $ do
    AibInstallationKid_Process kid <- idR
    CtxOne siteType <- contextT 
    let groupName = ""
    (funCode, _) <- codeMapping2 (siteType, groupName)
    makeS4Function funCode

makeS4Function :: MonadThrow m => String -> m S4.S4Function
makeS4Function funCode = do
    funName <- level2FunctionDescription funCode
    return $ S4.S4Function
        { S4.function_floc_code      = ""
        , S4.function_code           = funCode
        , S4.function_name           = funName
        , S4.function_attributes     = noAttrs
        , S4.function_kids           = []
        }

processGroup :: TransformE AibProcessGroup S4.S4ProcessGroup
processGroup = do
    group@AibProcessGroup {} <- idR
    let groupName = process_group_name group
    CtxOne siteType <- contextT
    (_, pgCode) <- codeMapping2 (siteType, groupName)
    pgDescr <- level3ProcessGroupDescription pgCode
    allprocs <- liftContext (push groupName) $ aibProcessGroupT processGroupKid (\_ _ _ kids -> kids)

    return $ S4.S4ProcessGroup 
                { S4.process_group_floc_code    = ""
                , S4.process_group_code         = pgCode
                , S4.process_group_name         = pgDescr
                , S4.process_group_attributes   = noAttrs
                , S4.process_group_kids         = allprocs
                }

processGroupKid :: TransformE AibProcessGroupKid S4.S4Process
processGroupKid = processGroupKid_Process
    
processGroupKid_Process :: TransformE AibProcessGroupKid S4.S4Process
processGroupKid_Process = 
    aibProcessGroupKid_ProcessT process (\a -> a)

process :: TransformE AibProcess S4.S4Process
process = do
    proc@AibProcess {} <- idR
    let procName = process_name proc
    CtxTwo siteType groupName <- contextT
    (_, _, procCode) <- codeMapping3 (siteType, groupName, procName)
    procDescr <- level4ProcessDescription procCode
    return $ S4.S4Process
                { S4.process_floc_code          = ""
                , S4.process_code               = procCode
                , S4.process_name               = procDescr
                , S4.process_attributes         = noAttrs
                , S4.process_kids               = []
                }

 