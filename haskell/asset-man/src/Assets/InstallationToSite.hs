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

import Assets.Facts.CodeMapping   
import Assets.Facts.CodeNames
import Assets.Common
import Assets.AibTypes
import qualified Assets.S4Types as S4

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