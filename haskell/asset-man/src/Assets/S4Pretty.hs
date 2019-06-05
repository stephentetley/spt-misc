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


demo01 = drawTree $ Node "one" [Node "two" [], Node "three" []]


type TransformE a b = Transform () KureM a b


applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t ()



s4DrawTree :: S4Site -> String
s4DrawTree site = 
    case applyTransform  siteTree site of
        Left err -> err
        Right tree -> drawTree tree


siteTree :: TransformE S4Site (Tree String)
siteTree = do
    site@S4Site {} <- idR
    kids <- s4SiteT functionTree (\_ _ _ -> id)
    return $ Node (site_name site) kids

functionTree ::  TransformE S4Function (Tree String)   
functionTree = do
    func@S4Function {} <- idR
    kids <- s4FunctionT processGroupTree (\_ _ _ _ -> id)
    return $ Node (function_name func) kids

processGroupTree ::  TransformE S4ProcessGroup (Tree String)   
processGroupTree = do
    procg@S4ProcessGroup {} <- idR
    kids <- s4ProcessGroupT processTree (\_ _ _ _ -> id)
    return $ Node (process_group_name procg) kids    


processTree ::  TransformE S4Process (Tree String)   
processTree = do
    proc@S4Process {} <- idR
    kids <- s4ProcessT systemTree (\_ _ _ _ -> id)
    return $ Node (process_name proc) kids        



systemTree ::  TransformE S4System (Tree String)   
systemTree = do
    sys@S4System {} <- idR
    kids <- s4SystemT subsystemTree (\_ _ _ _ -> id)
    return $ Node (system_name sys) kids        


subsystemTree ::  TransformE S4Subsystem (Tree String)   
subsystemTree = do
    subsys@S4Subsystem {} <- idR
    kids <- s4SubsystemT mainItemTree (\_ _ _ _ -> id)
    return $ Node (subsystem_name subsys) kids 

mainItemTree ::  TransformE S4MainItem (Tree String)   
mainItemTree = do
    item@S4MainItem {} <- idR
    kids <- s4MainItemT componentTree (\_ _ _ _ -> id)
    return $ Node (main_item_name item) kids 
    
componentTree ::  TransformE S4Component (Tree String)   
componentTree = do
    comp@S4Component {} <- idR
    return $ Node (component_name comp) [] 