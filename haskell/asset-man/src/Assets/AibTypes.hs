{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.AibTypes
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Aib Types.
--
--------------------------------------------------------------------------------



module Assets.AibTypes
  ( 
    AibInstallation(..)
  , AibProcessGroup(..)
  , AibProcess(..)
  , AibPlantAssembly(..)
  , AibPlantItem(..)
  , AibEquipment(..)
  ) where
  
import Assets.Common

data AibInstallation = AibInstallation
    { installation_ref          :: String
    , installation_name         :: String
    , installation_type         :: String
    , installation_attributes   :: Attributes
    , installation_kids         :: [String]
    }
    deriving (Eq,Ord,Show)
    
data AibProcessGroup = AibProcessGroup
    { process_group_ref          :: String
    , process_group_name         :: String
    , process_group_attributes   :: Attributes
    , process_group_kids         :: [String]
    }
    deriving (Eq,Ord,Show)
    
data AibProcess = AibProcess
    { process_ref          :: String
    , process_name         :: String
    , process_attributes   :: Attributes
    , process_kids         :: [String]
    }
    deriving (Eq,Ord,Show)
      
data AibPlantAssembly = AibPlantAssembly
    { plant_assembly_ref          :: String
    , plant_assembly_name         :: String
    , plant_assembly_attributes   :: Attributes
    , plant_assembly_kids         :: [String]
    }
    deriving (Eq,Ord,Show)
    
data AibPlantItem = AibPlantItem
    { plant_item_ref          :: String
    , plant_item_name         :: String
    , plant_item_attributes   :: Attributes
    , plant_item_kids         :: [String]
    }
    deriving (Eq,Ord,Show)
        
data AibEquipment= AibEquipment
    { equipment_ref          :: String
    , equipment_name         :: String
    , equipment_attributes   :: Attributes
    , equipment_kids         :: [String]
    }
    deriving (Eq,Ord,Show)
