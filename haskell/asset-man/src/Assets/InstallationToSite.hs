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

-- import Assets.Facts.CodeMapping   
-- import Assets.Facts.CodeNames
import Assets.Facts.SiteNameMapping
import Assets.Common
import Assets.AibTypes
import Assets.AibUniverse
import qualified Assets.S4Types as S4

type TransformE a b = Transform () KureM a b
type RewriteE a b = TransformE a b

applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty



installationToSite :: TransformE AibInstallation S4.S4Site
installationToSite = installation


installation :: TransformE AibInstallation S4.S4Site
installation = do 
    inst@AibInstallation {} <- idR
    info <- siteNameMapping (installation_ref inst)
    allfuns <- aibInstallationT installationKid (\_ _ _ _ kids -> kids)
    return S4.S4Site 
              { S4.site_code           = floc1 info
              , S4.site_name           = site_name info
              , S4.site_attributes     = installation_attributes inst
              , S4.site_kids           = S4.coalesceFunctions allfuns
              }

installationKid :: TransformE AibInstallationKid S4.S4Function
installationKid = 
    installationKid_ProcessGroup <+ installationKid_Process
  where
    installationKid_ProcessGroup = installationKid_TEMP
    installationKid_Process = installationKid_TEMP

installationKid_TEMP :: TransformE AibInstallationKid S4.S4Function
installationKid_TEMP = do
    return S4.S4Function
              { S4.function_floc_code      = "TODO"
              , S4.function_code           = "TODO"
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