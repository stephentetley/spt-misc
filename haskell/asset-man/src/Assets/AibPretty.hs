{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Asset.AibPretty
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Aib Pretty print.
--
--------------------------------------------------------------------------------



module Assets.AibPretty 
    (
        aibDrawTree  
    ) where

import Data.Tree

import Language.KURE                    -- package: KURE

import Assets.AibTypes
import Assets.AibUniverse



type TransformE a b = Transform () KureM a b


applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t ()



aibDrawTree :: AibInstallation -> String
aibDrawTree installation = 
    case applyTransform  installationTree installation of
        Left err -> err
        Right tree -> drawTree tree


installationTree :: TransformE AibInstallation (Tree String)
installationTree = do
    inst@AibInstallation {} <- idR
    kids <- return [] -- s4SiteT functionTree (\_ _ _ -> id)
    return $ Node (installation_name inst) kids

    {-
installationTree_ProcessGroup ::  TransformE AibInstallationKid (Tree String)   
installationTree_ProcessGroup = do
    procg@AibInstallationKid_ProcessGroup {} <- idR
    kids <- return [] -- s4FunctionT processGroupTree (\_ _ _ _ -> id)
    return $ Node (function_name func) kids


processGroupTree ::  TransformE AibProcessGroup (Tree String)   
processGroupTree = do
    procg@AibProcessGroup {} <- idR
    kids <- aibProcessGroupT processTree (\_ _ _ _ -> id)
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

-}    