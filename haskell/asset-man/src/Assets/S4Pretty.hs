{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Asset.S4Pretty
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- S4 Pretty print.
--
--------------------------------------------------------------------------------



module Assets.S4Pretty 
    (
        s4DrawTree  
    ) where

import Data.Tree

import Language.KURE                    -- package: KURE

import Assets.S4Types
import Assets.S4Universe

nodeLabel :: String -> String -> String -> String
nodeLabel name code typ = name ++ " (" ++ code ++ ") : " ++ typ


type TransformE a b = Transform IgnorePath KureM a b


applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty



s4DrawTree :: S4Site -> String
s4DrawTree site = 
    case applyTransform  siteTree site of
        Left err -> err
        Right tree -> drawTree tree


siteTree :: TransformE S4Site (Tree String)
siteTree = do
    S4Site { site_name = name, site_code = code } <- idR
    kids <- s4SiteT functionTree (\_ _ _ -> id)
    return $ Node (nodeLabel name code "Site") kids

functionTree ::  TransformE S4Function (Tree String)   
functionTree = do
    S4Function { function_name = name, function_code = code } <- idR
    kids <- s4FunctionT processGroupTree (\_ _ _ _ -> id)
    return $ Node (nodeLabel name code "Function") kids

processGroupTree ::  TransformE S4ProcessGroup (Tree String)   
processGroupTree = do
    S4ProcessGroup { process_group_name = name, process_group_code = code } <- idR
    kids <- s4ProcessGroupT processTree (\_ _ _ _ -> id)
    return $ Node (nodeLabel name code "ProcessGroup") kids    


processTree ::  TransformE S4Process (Tree String)   
processTree = do
    S4Process { process_name = name, process_code = code } <- idR
    kids <- s4ProcessT systemTree (\_ _ _ _ -> id)
    return $ Node (nodeLabel name code "Process") kids        



systemTree ::  TransformE S4System (Tree String)   
systemTree = do
    S4System { system_name = name, system_code = code } <- idR
    kids <- s4SystemT subsystemTree (\_ _ _ _ -> id)
    return $ Node (nodeLabel name code "System") kids        


subsystemTree ::  TransformE S4Subsystem (Tree String)   
subsystemTree = do
    S4Subsystem { subsystem_name = name, subsystem_code = code } <- idR
    kids <- s4SubsystemT mainItemTree (\_ _ _ _ -> id)
    return $ Node (nodeLabel name code "Subsystem") kids 

mainItemTree ::  TransformE S4MainItem (Tree String)   
mainItemTree = do
    S4MainItem { main_item_name = name, main_item_code = code } <- idR
    kids <- s4MainItemT componentTree (\_ _ _ _ -> id)
    return $ Node (nodeLabel name code "MaintainableItem") kids 
    
componentTree ::  TransformE S4Component (Tree String)   
componentTree = do
    S4Component { component_name = name, component_code = code } <- idR
    return $ Node (nodeLabel name code"Component") [] 