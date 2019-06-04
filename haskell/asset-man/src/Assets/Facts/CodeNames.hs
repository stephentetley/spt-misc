{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.Facts.CodeNames
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Map code to Name
--
--------------------------------------------------------------------------------



module Assets.Facts.CodeNames
    (
        level2FunctionDescription
    ) where

import Prelude hiding ( fail )        
import Control.Monad.Fail                   -- package: base

-- level2_function_description(code:atom, decription:atom).
level2FunctionDescription :: MonadFail m => String -> m String
level2FunctionDescription "CAA"     = return "Control and Automation"
level2FunctionDescription "E"       = return "Electrical Power Supply"
level2FunctionDescription _key      = fail ("unknown code: " ++ _key)