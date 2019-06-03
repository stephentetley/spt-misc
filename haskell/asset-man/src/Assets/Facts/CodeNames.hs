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



module Assets.Facts.CodeNames where


-- level2_function_description(code:atom, decription:atom).
level2FunctionDescription :: String -> String
level2FunctionDescription "CAA" = "Control and Automation"
level2FunctionDescription "E"   = "Electrical Power Supply"
level2FunctionDescription _     = "<unknown>"