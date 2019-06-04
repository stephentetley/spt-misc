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
      codeMapping3
    , codeMapping2
    ) where

import Prelude hiding ( fail )        
import Control.Monad.Fail                   -- package: base


-- | (TypeName, ProcessGroupName, ProcessName) -> (FunctionCode, ProcGroupCode, ProcCode)
--
codeMapping3 :: MonadFail m => (String, String, String) -> m (String, String, String)
codeMapping3 ("SPS", "", "SEWAGE PUMPING") = return ("", "", "PMG")
codeMapping3 ("STW", "", "SEWAGE PUMPING") = return ("", "", "PMG")
codeMapping3 (_,_,_) = fail "codeMapping3 - no mapping"


-- NOTE
-- I think we can filter the rules when generating this table to
-- make a deterministic 'codeMapping2 

-- | (TypeName, ProcessGroupName) -> (FunctionCode, ProcGroupCode)
--
codeMapping2 :: MonadFail m => (String, String) -> m (String, String)
codeMapping2 ("SPS", "") = return ("", "")
codeMapping2 ("STW", "") = return ("", "")
codeMapping2 ("STW", _)   = return ("", "")     -- HACK
codeMapping2 (_,_) = fail "codeMapping2 - no mapping"


