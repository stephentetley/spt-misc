{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.FlocLabelling
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- S4 pass to generate FLOC paths.
--
--------------------------------------------------------------------------------



module Assets.FlocLabelling 

    where


import Language.KURE                    -- package: KURE

import Assets.FlocPath
import Assets.S4Types
import Assets.S4Universe
import Assets.TranslateMonad

type FlocTransform  a b = Transform FlocPath TranslateM a b
type FlocRewrite    a b = FlocTransform a b

