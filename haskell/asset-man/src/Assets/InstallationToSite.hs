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

-- import Control.Monad      

import Language.KURE                    -- package: KURE

import Assets.Facts.CodeMapping   
-- import Assets.Facts.CodeNames
import Assets.Facts.SiteNameMapping
import Assets.Common
import Assets.FlocPath
import Assets.AibTypes
import Assets.AibUniverse
import qualified Assets.S4Types as S4

-- TODO - context should be FlocCode

type TransformE a b = Transform FlocPath KureM a b
type RewriteE a b = TransformE a b

applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty



installationToSite :: TransformE AibInstallation S4.S4Site
installationToSite = installation


installation :: TransformE AibInstallation S4.S4Site
installation = do 
    inst@AibInstallation {} <- idR
    info <- siteNameMapping (installation_ref inst)
    let siteType = site_type info
    allfuns <- aibInstallationT (installationKid siteType) (\_ _ _ _ kids -> kids)
    return S4.S4Site 
              { S4.site_code           = floc1 info
              , S4.site_name           = site_name info
              , S4.site_attributes     = installation_attributes inst
              , S4.site_kids           = S4.coalesceFunctions allfuns
              }

installationKid :: String -> TransformE AibInstallationKid S4.S4Function
installationKid siteType = 
    installationKid_ProcessGroup siteType 
      <+ installationKid_Process siteType

installationKid_ProcessGroup :: String -> TransformE AibInstallationKid S4.S4Function      
installationKid_ProcessGroup siteType = withPatFailExc (strategyFailure "ProcessGroup") $ do
    AibInstallationKid_ProcessGroup kid <- idR
    let groupName = process_group_name kid
    (funCode, _) <- codeMapping2 (siteType, groupName)
    return $ makeS4Function funCode

installationKid_Process :: String -> TransformE AibInstallationKid S4.S4Function  
installationKid_Process siteType = withPatFailExc (strategyFailure "Process") $ do
    AibInstallationKid_Process kid <- idR
    let groupName = ""
    (funCode, _) <- codeMapping2 (siteType, groupName)
    return $ makeS4Function funCode

makeS4Function :: String -> S4.S4Function
makeS4Function funCode = 
    S4.S4Function
        { S4.function_floc_code      = "TODO"
        , S4.function_code           = funCode
        , S4.function_name           = "TODO"
        , S4.function_attributes     = noAttrs
        , S4.function_kids           = []
        }

{-
generateS4ProcessNode level1code instType (procgKey, procNode) = 
    S4.S4Process 
        { S4.process_floc_code             = ""
        , S4.process_code                  = level4Code
        , S4.process_name                  = ""
        , S4.process_attributes            = noAttrs
        , S4.process_kids                  = []
        }
  where
    procKey = process_ref procNode
    Just (level2Code, level3Code, level4Code) = codeMapping (level1code, procgKey, procKey)

-}    