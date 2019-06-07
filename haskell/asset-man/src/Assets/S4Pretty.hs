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

import Assets.FlocPath

nodeLabel :: String -> String -> String -> String
nodeLabel name code typ = name ++ " (" ++ code ++ ") : " ++ typ


type TransformPretty a b = Transform FlocPath KureM a b


applyTransform :: TransformPretty a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty



s4DrawTree :: S4Site -> String
s4DrawTree site = 
    case applyTransform  siteTree site of
        Left err -> err
        Right tree -> drawTree tree


siteTree :: TransformPretty S4Site (Tree String)
siteTree = do
    S4Site { site_name = name, site_floc = floc } <- idR
    kids <- s4SiteT functionTree (\_ _ _ -> id)
    return $ Node (nodeLabel name floc "Site") kids


functionTree :: TransformPretty S4Function (Tree String)   
functionTree = do
    S4Function { function_name = name, function_floc = floc } <- idR
    kids <- s4FunctionT processGroupTree (\_ _ _ -> id)
    return $ Node (nodeLabel name floc "Function") kids


processGroupTree :: TransformPretty S4ProcessGroup (Tree String)   
processGroupTree = do
    S4ProcessGroup { process_group_name = name, process_group_floc = floc } <- idR
    kids <- s4ProcessGroupT processTree (\_ _ _ -> id)
    return $ Node (nodeLabel name floc "ProcessGroup") kids    


processTree :: TransformPretty S4Process (Tree String)   
processTree = do
    S4Process { process_name = name, process_floc = floc } <- idR
    kids <- s4ProcessT systemTree (\_ _ _ -> id)
    return $ Node (nodeLabel name floc "Process") kids        



systemTree ::  TransformPretty S4System (Tree String)   
systemTree = do
    S4System { system_name = name, system_floc = floc } <- idR
    kids <- s4SystemT subsystemTree (\_ _ _ -> id)
    return $ Node (nodeLabel name floc "System") kids        


subsystemTree ::  TransformPretty S4Subsystem (Tree String)   
subsystemTree = do
    S4Subsystem { subsystem_name = name, subsystem_floc = floc } <- idR
    kids <- s4SubsystemT mainItemTree (\_ _ _ -> id)
    return $ Node (nodeLabel name floc "Subsystem") kids 

mainItemTree ::  TransformPretty S4MainItem (Tree String)   
mainItemTree = do
    S4MainItem { main_item_name = name, main_item_floc = floc } <- idR
    kids <- s4MainItemT componentTree (\_ _ _ -> id)
    return $ Node (nodeLabel name floc "MaintainableItem") kids 
    
componentTree ::  TransformPretty S4Component (Tree String)   
componentTree = do
    S4Component { component_name = name, component_floc = floc } <- idR
    return $ Node (nodeLabel name floc "Component") [] 