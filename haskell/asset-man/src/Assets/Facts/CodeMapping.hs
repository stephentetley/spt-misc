{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.Facts.CodeMapping
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Encoding the mapping rules.
--
--------------------------------------------------------------------------------



module Assets.Facts.CodeMapping 
    (
      codeMapping
    ) where

import Prelude hiding ( fail )        
import Control.Monad.Fail                   -- package: base

codeMapping :: MonadFail m => (String, String, String) -> m (String, String, String)
codeMapping ("SPS", "", "SEWAGE PUMPING") = return ("", "", "PMG")
codeMapping (_,_,_) = fail "no mapping"


