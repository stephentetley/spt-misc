{-# LANGUAGE LambdaCase                 #-}
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

nodeLabel :: String -> String -> String
nodeLabel name typ = name ++ " : " ++ typ

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
    kids <- aibInstallationT installationKid (\_ _ _ _ -> id)
    return $ Node (nodeLabel (installation_name inst) "Installation") kids



-- Don't draw    
installationKid ::  TransformE AibInstallationKid (Tree String)   
installationKid = 
    transform $ \cx -> \case
        AibInstallationKid_ProcessGroup kid -> applyT processGroupTree cx kid
        AibInstallationKid_Process kid -> applyT processTree cx kid




processGroupTree ::  TransformE AibProcessGroup (Tree String)   
processGroupTree = do
    procg@AibProcessGroup {} <- idR
    kids <- aibProcessGroupT processGroupKid (\_ _ _ -> id)
    return $ Node (nodeLabel (process_group_name procg) "ProcessGroup") kids    


-- Don't draw    
processGroupKid ::  TransformE AibProcessGroupKid (Tree String)   
processGroupKid = 
    transform $ \cx -> \case
        AibProcessGroupKid_Process kid -> applyT processTree cx kid


processTree ::  TransformE AibProcess (Tree String)   
processTree = do
    proc@AibProcess {} <- idR
    kids <- aibProcessT processKid (\_ _ _ -> id)
    return $ Node (nodeLabel (process_name proc) "Process") kids        

processKid :: TransformE AibProcessKid (Tree String)      
processKid = 
    transform $ \cx -> \case
        AibProcessKid_PlantAssembly kid -> applyT plantAssemblyTree cx kid
        AibProcessKid_PlantItem kid     -> applyT plantItemTree cx kid


plantAssemblyTree ::  TransformE AibPlantAssembly (Tree String)   
plantAssemblyTree = do
    item@AibPlantAssembly {} <- idR
    kids <- aibPlantAssemblyT plantAssemblyKid (\_ _ _ -> id)
    return $ Node (nodeLabel (plant_assembly_name item) "PlantAssembly") kids    

plantAssemblyKid :: TransformE AibPlantAssemblyKid  (Tree String) 
plantAssemblyKid = 
    transform $ \cx -> \case
        AibPlantAssemblyKid_PlantItem kid   -> applyT plantItemTree cx kid
        AibPlantAssemblyKid_Equipment kid   -> applyT equipmentTree cx kid
        AibPlantAssemblyKid_Unknown kid     -> applyT unknownTree cx kid
    
    
plantItemTree ::  TransformE AibPlantItem (Tree String)   
plantItemTree = do
    item@AibPlantItem {} <- idR
    kids <- aibPlantItemT equipmentTree (\_ _ _ -> id)
    return $ Node (nodeLabel (plant_item_name item) "PlantItem") kids   
    
    
equipmentTree ::  TransformE AibEquipment (Tree String)   
equipmentTree = do
    item@AibEquipment {} <- idR
    return $ Node (nodeLabel (equipment_name item) "Equipment") []

unknownTree ::  TransformE AibUnknown (Tree String)   
unknownTree = do
    item@AibUnknown {} <- idR
    return $ Node (nodeLabel (unknown_name item) "Unknown") []    