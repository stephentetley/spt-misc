{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE DoAndIfThenElse            #-}
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
    , AibInstallationKid(..)
    , AibProcessGroup(..)
    , AibProcessGroupKid(..)
    , AibProcess(..)
    , AibProcessKid(..)
    , AibPlantAssembly(..)
    , AibPlantAssemblyKid(..)
    , AibPlantItem(..)
    , AibEquipment(..)
    , AibUnknown(..)

    

    ) where

import Text.JSON                        -- package: json

import Assets.Common

data AibInstallation = AibInstallation
    { installation_ref          :: String
    , installation_name         :: String
    , installation_type         :: String
    , installation_attributes   :: Attributes
    , installation_kids         :: [AibInstallationKid]
    }
    deriving (Eq,Ord,Show)

data AibInstallationKid 
    = AibInstallationKid_ProcessGroup  AibProcessGroup  
    | AibInstallationKid_Process       AibProcess
    deriving (Eq,Ord,Show)

data AibProcessGroup = AibProcessGroup
    { process_group_ref          :: String
    , process_group_name         :: String
    , process_group_attributes   :: Attributes
    , process_group_kids         :: [AibProcessGroupKid]
    }
    deriving (Eq,Ord,Show)


data AibProcessGroupKid 
    = AibProcessGroupKid_Process       AibProcess
    deriving (Eq,Ord,Show)

    
data AibProcess = AibProcess
    { process_ref          :: String
    , process_name         :: String
    , process_attributes   :: Attributes
    , process_kids         :: [AibProcessKid]
    }
    deriving (Eq,Ord,Show)


data AibProcessKid 
    = AibProcessKid_PlantAssembly      AibPlantAssembly
    | AibProcessKid_PlantItem          AibPlantItem
    deriving (Eq,Ord,Show)

data AibPlantAssembly = AibPlantAssembly
    { plant_assembly_ref          :: String
    , plant_assembly_name         :: String
    , plant_assembly_attributes   :: Attributes
    , plant_assembly_kids         :: [AibPlantAssemblyKid]
    }
    deriving (Eq,Ord,Show)

    
data AibPlantAssemblyKid 
    = AibPlantAssemblyKid_PlantItem         AibPlantItem
    | AibPlantAssemblyKid_Equipment         AibEquipment
    | AibPlantAssemblyKid_Unknown           AibUnknown
    deriving (Eq,Ord,Show)

data AibPlantItem = AibPlantItem
    { plant_item_ref          :: String
    , plant_item_name         :: String
    , plant_item_attributes   :: Attributes
    , plant_item_kids         :: [AibEquipment]
    }
    deriving (Eq,Ord,Show)
        
data AibEquipment = AibEquipment
    { equipment_ref          :: String
    , equipment_name         :: String
    , equipment_attributes   :: Attributes
    }
    deriving (Eq,Ord,Show)

        
data AibUnknown = AibUnknown
    { unknown_ref          :: String
    , unknown_name         :: String
    , unknown_attributes   :: Attributes
    }
    deriving (Eq,Ord,Show)

-------------------------------------------------------------------------------
-- JSON


instance JSON AibInstallation where
    showJSON :: AibInstallation -> JSValue
    showJSON a@(AibInstallation {}) = JSObject $ toJSObject $
        [ ("assetReference",        showJSON $ installation_ref a)
        , ("attributes",            attributesToJSON $ installation_attributes a)
        , ("kids",                  JSArray (map showJSON $ installation_kids a))
        , ("name",                  showJSON $ installation_name a)
        , ("type",                  showJSON $ installation_type a)
        ]
  
    readJSON :: JSValue -> Result AibInstallation
    readJSON (JSObject obj) = do 
        code      <- valFromObj "assetReference"    obj >>= readJSON
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "Installation" then 
            return (AibInstallation 
                        { installation_ref          = code
                        , installation_name         = name
                        , installation_type         = itemtype 
                        , installation_attributes   = attrs
                        , installation_kids         = kids
                        })
        else
            fail "Not an installation"
  
    readJSON _ = Error "Not an installation"    

instance JSON AibInstallationKid where
    showJSON :: AibInstallationKid -> JSValue    
    showJSON (AibInstallationKid_ProcessGroup prcg) = showJSON prcg  
    showJSON (AibInstallationKid_Process prc)       = showJSON prc

    readJSON :: JSValue -> Result AibInstallationKid
    readJSON outer@(JSObject obj) = do 
        itemtype  <- valFromObj "type"        obj >>= readJSON
        case itemtype of
            "ProcessGroup" -> AibInstallationKid_ProcessGroup <$> readJSON outer 
            "Process" -> AibInstallationKid_Process <$> readJSON outer 
            _  -> fail "unknown Installation child"

    readJSON _ = Error "Not an installation kid"                

instance JSON AibProcessGroup where
    showJSON :: AibProcessGroup -> JSValue
    showJSON a@(AibProcessGroup {}) = JSObject $ toJSObject $
        [ ("assetReference",        showJSON $ process_group_ref a)
        , ("attributes",            attributesToJSON $ process_group_attributes a)
        , ("kids",                  JSArray (map showJSON $ process_group_kids a))
        , ("name",                  showJSON $ process_group_name a)
        , ("type",                  showJSON "ProcessGroup")
        ]
    
    readJSON :: JSValue -> Result AibProcessGroup
    readJSON (JSObject obj) = do 
        code      <- valFromObj "assetReference"    obj >>= readJSON
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "ProcessGroup" then 
            return (AibProcessGroup 
                        { process_group_ref          = code
                        , process_group_name         = name
                        , process_group_attributes   = attrs
                        , process_group_kids         = kids
                        })
        else
            fail "Not an process group"
    
    readJSON _ = Error "Not an process group"        


instance JSON AibProcessGroupKid where
    showJSON :: AibProcessGroupKid -> JSValue    
    showJSON (AibProcessGroupKid_Process prc)       = showJSON prc

    readJSON :: JSValue -> Result AibProcessGroupKid
    readJSON outer@(JSObject obj) = do 
        itemtype  <- valFromObj "type"        obj >>= readJSON
        case itemtype of
            "Process" -> AibProcessGroupKid_Process <$> readJSON outer
            _  -> fail ("unknown Process Group kid " ++ itemtype)

    readJSON _ = Error "Not a Process Group kid"     


instance JSON AibProcess where
    showJSON :: AibProcess -> JSValue
    showJSON a@(AibProcess {}) = JSObject $ toJSObject $
        [ ("assetReference",        showJSON $ process_ref a)
        , ("attributes",            attributesToJSON $ process_attributes a)
        , ("kids",                  JSArray (map showJSON $ process_kids a))
        , ("name",                  showJSON $ process_name a)
        , ("type",                  showJSON "Process")
        ]
    
    readJSON :: JSValue -> Result AibProcess
    readJSON (JSObject obj) = do 
        code      <- valFromObj "assetReference"    obj >>= readJSON
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "Process" then 
            return (AibProcess 
                        { process_ref          = code
                        , process_name         = name
                        , process_attributes   = attrs
                        , process_kids         = kids
                        })
        else
            fail ("Not a process " ++ itemtype ++ show obj)
    
    readJSON x = Error ("Not a process " ++ show x)


instance JSON AibProcessKid where
    showJSON :: AibProcessKid -> JSValue    
    showJSON (AibProcessKid_PlantAssembly prc)     = showJSON prc
    showJSON (AibProcessKid_PlantItem item)        = showJSON item

    readJSON :: JSValue -> Result AibProcessKid
    readJSON outer@(JSObject obj) = do 
        itemtype  <- valFromObj "type"        obj >>= readJSON
        case itemtype of
            "PlantAssembly" -> AibProcessKid_PlantAssembly <$> readJSON outer
            "PlantItem" -> AibProcessKid_PlantItem <$> readJSON outer
            _  -> fail ("unknown Process kid " ++ itemtype)

    readJSON _ = Error "Not a Process kid"     

        
instance JSON AibPlantAssembly where
    showJSON :: AibPlantAssembly -> JSValue
    showJSON a@(AibPlantAssembly {}) = JSObject $ toJSObject $
        [ ("assetReference",        showJSON $ plant_assembly_ref a)
        , ("attributes",            attributesToJSON $ plant_assembly_attributes a)
        , ("kids",                  JSArray (map showJSON $ plant_assembly_kids a))
        , ("name",                  showJSON $ plant_assembly_name a)
        , ("type",                  showJSON "PlantAssembly")
        ]
    
    readJSON :: JSValue -> Result AibPlantAssembly
    readJSON (JSObject obj) = do 
        code      <- valFromObj "assetReference"    obj >>= readJSON
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "PlantAssembly" then 
            return (AibPlantAssembly 
                        { plant_assembly_ref          = code
                        , plant_assembly_name         = name
                        , plant_assembly_attributes   = attrs
                        , plant_assembly_kids         = kids
                        })
        else
            fail "Not a plant assembly"
    
    readJSON _ = Error "Not a plant assembly"  


instance JSON AibPlantAssemblyKid where
    showJSON :: AibPlantAssemblyKid -> JSValue    
    showJSON (AibPlantAssemblyKid_PlantItem item)       = showJSON item  
    showJSON (AibPlantAssemblyKid_Equipment equip)      = showJSON equip
    showJSON (AibPlantAssemblyKid_Unknown ukwn)         = showJSON ukwn

    readJSON :: JSValue -> Result AibPlantAssemblyKid
    readJSON outer@(JSObject obj) = do 
        itemtype  <- valFromObj "type"        obj >>= readJSON
        case itemtype of
            "PlantItem" -> AibPlantAssemblyKid_PlantItem <$> readJSON outer 
            "Equipment" -> AibPlantAssemblyKid_Equipment <$> readJSON outer 
            "Undefined" -> AibPlantAssemblyKid_Unknown   <$> readJSON outer 
            _  -> fail ("unknown Plant Assembly child " ++ itemtype)

    readJSON _ = Error "Not a Plant Assembly kid"   

instance JSON AibPlantItem where
    showJSON :: AibPlantItem -> JSValue
    showJSON a@(AibPlantItem {}) = JSObject $ toJSObject $
        [ ("assetReference",        showJSON $ plant_item_ref a)
        , ("attributes",            attributesToJSON $ plant_item_attributes a)
        , ("kids",                  JSArray (map showJSON $ plant_item_kids a))
        , ("name",                  showJSON $ plant_item_name a)
        , ("type",                  showJSON "PlantItem")
        ]
    
    readJSON :: JSValue -> Result AibPlantItem
    readJSON (JSObject obj) = do 
        code      <- valFromObj "assetReference"    obj >>= readJSON
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "PlantItem" then 
            return (AibPlantItem 
                        { plant_item_ref          = code
                        , plant_item_name         = name
                        , plant_item_attributes   = attrs
                        , plant_item_kids         = kids
                        })
        else
            fail "Not a plant item"
    
    readJSON _ = Error "Not a plant item"  
    


instance JSON AibEquipment where
    showJSON :: AibEquipment -> JSValue
    showJSON a@(AibEquipment {}) = JSObject $ toJSObject $
        [ ("assetReference",        showJSON $ equipment_ref a)
        , ("attributes",            attributesToJSON $ equipment_attributes a)
        , ("kids",                  JSArray [])
        , ("name",                  showJSON $ equipment_name a)
        , ("type",                  showJSON "Equipment")
        ]
    
    readJSON :: JSValue -> Result AibEquipment
    readJSON (JSObject obj) = do 
        code      <- valFromObj "assetReference"    obj >>= readJSON
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "Equipment" then 
            return (AibEquipment 
                        { equipment_ref          = code
                        , equipment_name         = name
                        , equipment_attributes   = attrs
                        })
        else
            fail "Not an equipment"
    
    readJSON _ = Error "Not an equipment"      


instance JSON AibUnknown where
    showJSON :: AibUnknown -> JSValue
    showJSON a@(AibUnknown {}) = JSObject $ toJSObject $
        [ ("assetReference",        showJSON $ unknown_ref a)
        , ("attributes",            attributesToJSON $ unknown_attributes a)
        , ("kids",                  JSArray [])
        , ("name",                  showJSON $ unknown_name a)
        , ("type",                  showJSON "Undefined")
        ]
    
    readJSON :: JSValue -> Result AibUnknown
    readJSON (JSObject obj) = do 
        code      <- valFromObj "assetReference"    obj >>= readJSON
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "Undefined" then 
            return (AibUnknown
                        { unknown_ref          = code
                        , unknown_name         = name
                        , unknown_attributes   = attrs
                        })
        else
            fail "Not an Undefined"
    
    readJSON _ = Error "Not an Undefined"  
    
