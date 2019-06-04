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


import Language.KURE.MonadCatch         -- package: KURE

import Assets.Common


-- | (TypeName, ProcessGroupName, ProcessName) -> (FunctionCode, ProcGroupCode, ProcCode)
--
codeMapping3 :: MonadThrow m => (String, String, String) -> m (String, String, String)
codeMapping3 ("SPS", "", "SEWAGE PUMPING") = return ("", "", "PMG")
codeMapping3 ("STW", "", "SEWAGE PUMPING") = return ("", "", "PMG")
codeMapping3 (_,_,_) = throwM (LookupException $ "codeMapping3 - no mapping")


-- NOTE
-- I think we can filter the rules when generating this table to
-- make a deterministic 'codeMapping2' provided both parts of
-- the pair are non-blank.

-- | (TypeName, ProcessGroupName) -> (FunctionCode, ProcGroupCode)
--
codeMapping2 :: MonadThrow m => (String, String) -> m (String, String)
codeMapping2 ("SPS", "") = return ("", "")
codeMapping2 ("STW", "") = return ("", "")
codeMapping2 ("STW", _)   = return ("", "")     -- HACK
codeMapping2 (_,_) = throwM (LookupException $ "codeMapping2 - no mapping")


