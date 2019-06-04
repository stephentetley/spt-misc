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
    , level3ProcessGroupDescription
    , level4ProcessDescription
    ) where


import Language.KURE.MonadCatch         -- package: KURE

-- import Assets.Common

level2FunctionDescription :: MonadThrow m => String -> m String
level2FunctionDescription "CAA"     = return "Control and Automation"
level2FunctionDescription "E"       = return "Electrical Power Supply"
level2FunctionDescription key       = return $ "<To add: " ++ key ++ ">" 
-- level2FunctionDescription _key      = throwM (LookupException $ "unknown code: " ++ _key)

level3ProcessGroupDescription :: MonadThrow m => String -> m String
level3ProcessGroupDescription "CAA"     = return "Control and Automation"
level3ProcessGroupDescription "E"       = return "Electrical Power Supply"
level3ProcessGroupDescription key       = return $ "<To add: " ++ key ++ ">" 

level4ProcessDescription :: MonadThrow m => String -> m String
level4ProcessDescription "CAA"     = return "Control and Automation"
level4ProcessDescription "E"       = return "Electrical Power Supply"
level4ProcessDescription key       = return $ "<To add: " ++ key ++ ">" 