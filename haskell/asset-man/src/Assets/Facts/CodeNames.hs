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


import Language.KURE.MonadCatch         -- package: KURE

-- import Assets.Common

-- level2_function_description(code:atom, decription:atom).
level2FunctionDescription :: MonadThrow m => String -> m String
level2FunctionDescription "CAA"     = return "Control and Automation"
level2FunctionDescription "E"       = return "Electrical Power Supply"
level2FunctionDescription key       = return $ "<To add: " ++ key ++ ">" 
-- level2FunctionDescription _key      = throwM (LookupException $ "unknown code: " ++ _key)