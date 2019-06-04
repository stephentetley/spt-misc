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



module Assets.InstallationToSite where

import Language.KURE

import Assets.Facts.CodeMapping   
import Assets.Facts.CodeNames
import Assets.Facts.SiteNameMapping
import Assets.Common
import Assets.AibTypes
import qualified Assets.S4Types as S4

type TransformE a b = Transform () KureM a b
type RewriteE a b = TransformE a b

applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty


rootNode :: TransformE AibInstallation S4.S4Site
rootNode = do 
    inst@AibInstallation {} <- idR
    info <- siteNameMapping (installation_ref inst)
    return S4.S4Site 
              { S4.site_code           = floc1 info
              , S4.site_name           = site_name info
              , S4.site_attributes     = noAttrs
              , S4.site_kids           = []
              }



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