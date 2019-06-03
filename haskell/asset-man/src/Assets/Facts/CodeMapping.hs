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

-- Inputs look nicer as a tuple...

codeMapping :: (String, String, String) -> Maybe (String, String, String)
codeMapping ("SPS", "", "SEWAGE PUMPING") = Just ("", "", "PMG")
codeMapping (_,_,_) = Nothing
