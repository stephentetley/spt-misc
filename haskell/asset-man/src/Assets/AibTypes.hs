{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
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

    , AUniverse(..)

    , aibInstallationT
    , aibInstallationAllR
    , aibInstallationAnyR
    , aibInstallationOneR

    , aibInstallationKid_ProcessGroupT
    , aibInstallationKid_ProcessGroupAllR
    , aibInstallationKid_ProcessGroupAnyR
    , aibInstallationKid_ProcessGroupOneR  

    , aibInstallationKid_ProcessT
    , aibInstallationKid_ProcessAllR
    , aibInstallationKid_ProcessAnyR
    , aibInstallationKid_ProcessOneR  

    , aibProcessGroupT
    , aibProcessGroupAllR
    , aibProcessGroupAnyR
    , aibProcessGroupOneR

    , aibProcessGroupKid_ProcessT
    , aibProcessGroupKid_ProcessAllR
    , aibProcessGroupKid_ProcessAnyR
    , aibProcessGroupKid_ProcessOneR

    , aibProcessT
    , aibProcessAllR
    , aibProcessAnyR
    , aibProcessOneR

    , aibProcessKid_PlantAssemblyT
    , aibProcessKid_PlantAssemblyAllR
    , aibProcessKid_PlantAssemblyAnyR
    , aibProcessKid_PlantAssemblyOneR

    , aibProcessKid_PlantItemT
    , aibProcessKid_PlantItemAllR
    , aibProcessKid_PlantItemAnyR
    , aibProcessKid_PlantItemOneR

    , aibPlantAssemblyT
    , aibPlantAssemblyAllR
    , aibPlantAssemblyAnyR
    , aibPlantAssemblyOneR
    
    , aibPlantAssemblyKid_PlantItemT
    , aibPlantAssemblyKid_PlantItemAllR
    , aibPlantAssemblyKid_PlantItemAnyR
    , aibPlantAssemblyKid_PlantItemOneR

    , aibPlantAssemblyKid_EquipmentT
    , aibPlantAssemblyKid_EquipmentAllR
    , aibPlantAssemblyKid_EquipmentAnyR
    , aibPlantAssemblyKid_EquipmentOneR

    , aibPlantAssemblyKid_UnknownT
    , aibPlantAssemblyKid_UnknownAllR
    , aibPlantAssemblyKid_UnknownAnyR
    , aibPlantAssemblyKid_UnknownOneR

    , aibPlantItemT
    , aibPlantItemAllR
    , aibPlantItemAnyR
    , aibPlantItemOneR

    , aibEquipmentT
    , aibUnknownT

    ) where

import Text.JSON                        -- package: json
import Language.KURE                    -- package: kure

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
    
-------------------------------------------------------------------------------
-- KURE

data AUniverse = 
    AUInstallation              AibInstallation
  | AUInstallationKid           AibInstallationKid
  | AUProcessGroup              AibProcessGroup
  | AUProcessGroupKid           AibProcessGroupKid
  | AUProcess                   AibProcess
  | AUProcessKid                AibProcessKid
  | AUPlantAssembly             AibPlantAssembly
  | AUPlantAssemblyKid          AibPlantAssemblyKid 
  | AUPlantItem                 AibPlantItem
  | AUEquipment                 AibEquipment
  | AUUnknown                   AibUnknown


instance Injection AibInstallation AUniverse where
    inject = AUInstallation
  
    project (AUInstallation v) = Just v
    project _ = Nothing   

    
instance Injection AibInstallationKid AUniverse where
    inject = AUInstallationKid
  
    project (AUInstallationKid v) = Just v
    project _ = Nothing  

    
instance Injection AibProcessGroup AUniverse where
    inject = AUProcessGroup
    
    project (AUProcessGroup v) = Just v
    project _ = Nothing  

    
instance Injection AibProcessGroupKid AUniverse where
    inject = AUProcessGroupKid
    
    project (AUProcessGroupKid v) = Just v
    project _ = Nothing  

instance Injection AibProcess AUniverse where
    inject = AUProcess
    
    project (AUProcess v) = Just v
    project _ = Nothing  


instance Injection AibProcessKid AUniverse where
    inject = AUProcessKid
    
    project (AUProcessKid v) = Just v
    project _ = Nothing  

instance Injection AibPlantAssembly AUniverse where
    inject = AUPlantAssembly
    
    project (AUPlantAssembly v) = Just v
    project _ = Nothing 

instance Injection AibPlantAssemblyKid AUniverse where
    inject = AUPlantAssemblyKid
    
    project (AUPlantAssemblyKid v) = Just v
    project _ = Nothing 

instance Injection AibPlantItem AUniverse where
    inject = AUPlantItem
    
    project (AUPlantItem v) = Just v
    project _ = Nothing 


instance Injection AibEquipment AUniverse where
    inject = AUEquipment
    
    project (AUEquipment v) = Just v
    project _ = Nothing 
        
instance Injection AibUnknown AUniverse where
    inject = AUUnknown
    
    project (AUUnknown v) = Just v
    project _ = Nothing      
 
-------------------------------------------------------------------------------
-- KURE - congruence combinators

aibInstallationT :: (MonadThrow m) 
    => Transform c m AibInstallationKid a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m AibInstallation b
aibInstallationT t f = transform $ \c -> \case
    AibInstallation ref name typ attrs kids -> 
        f ref name typ attrs <$> mapM (\x -> applyT t c x) kids    

aibInstallationAllR :: (MonadThrow m) 
    => Rewrite c m AibInstallationKid -> Rewrite c m AibInstallation
aibInstallationAllR r1 = aibInstallationT r1 AibInstallation

aibInstallationAnyR :: (MonadCatch m) => Rewrite c m AibInstallationKid -> Rewrite c m AibInstallation
aibInstallationAnyR r1 = unwrapAnyR $ aibInstallationAllR (wrapAnyR r1) 

aibInstallationOneR :: (MonadCatch m) => Rewrite c m AibInstallationKid -> Rewrite c m AibInstallation
aibInstallationOneR r1 = unwrapOneR $ aibInstallationAllR (wrapOneR r1) 

-- For the Kid types, one set of congruence combinators for each constructor

aibInstallationKid_ProcessGroupT :: (MonadThrow m) 
    => Transform c m AibProcessGroup a1 
    -> (a1 -> b) 
    -> Transform c m AibInstallationKid b
aibInstallationKid_ProcessGroupT t f = transform $ \c -> \case
    AibInstallationKid_ProcessGroup obj -> 
        f <$> applyT t c obj
    _   -> throwM (nodeMismatch "AibInstallationKid_ProcessGroup") 
        
aibInstallationKid_ProcessGroupAllR :: (MonadThrow m) 
    => Rewrite c m AibProcessGroup -> Rewrite c m AibInstallationKid
aibInstallationKid_ProcessGroupAllR r1 = 
    aibInstallationKid_ProcessGroupT r1 AibInstallationKid_ProcessGroup

aibInstallationKid_ProcessGroupAnyR :: (MonadCatch m) 
    => Rewrite c m AibProcessGroup -> Rewrite c m AibInstallationKid
aibInstallationKid_ProcessGroupAnyR r1 = 
    unwrapAnyR $ aibInstallationKid_ProcessGroupAllR (wrapAnyR r1) 

aibInstallationKid_ProcessGroupOneR :: (MonadCatch m) 
    => Rewrite c m AibProcessGroup -> Rewrite c m AibInstallationKid
aibInstallationKid_ProcessGroupOneR r1 = 
    unwrapOneR $ aibInstallationKid_ProcessGroupAllR (wrapOneR r1) 

aibInstallationKid_ProcessT :: (MonadThrow m) 
    => Transform c m AibProcess a1 
    -> (a1 -> b) 
    -> Transform c m AibInstallationKid b
aibInstallationKid_ProcessT t f = transform $ \c -> \case
    AibInstallationKid_Process obj -> 
        f <$> applyT t c obj   
    _   -> throwM (nodeMismatch "AibInstallationKid_Process") 
        
aibInstallationKid_ProcessAllR :: (MonadThrow m) 
    => Rewrite c m AibProcess -> Rewrite c m AibInstallationKid
aibInstallationKid_ProcessAllR r1 = 
    aibInstallationKid_ProcessT r1 AibInstallationKid_Process

aibInstallationKid_ProcessAnyR :: (MonadCatch m) 
    => Rewrite c m AibProcess -> Rewrite c m AibInstallationKid
aibInstallationKid_ProcessAnyR r1 = 
    unwrapAnyR $ aibInstallationKid_ProcessAllR (wrapAnyR r1) 

aibInstallationKid_ProcessOneR :: (MonadCatch m) 
    => Rewrite c m AibProcess -> Rewrite c m AibInstallationKid
aibInstallationKid_ProcessOneR r1 = 
    unwrapOneR $ aibInstallationKid_ProcessAllR (wrapOneR r1)   


aibProcessGroupT :: (MonadThrow m) 
    => Transform c m AibProcessGroupKid a1 
    -> (String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m AibProcessGroup b
aibProcessGroupT t f = transform $ \c -> \case
    AibProcessGroup ref name attrs kids -> 
        f ref name attrs <$> mapM (\x -> applyT t c x) kids   

        
aibProcessGroupAllR :: (MonadThrow m) 
    => Rewrite c m AibProcessGroupKid -> Rewrite c m AibProcessGroup
aibProcessGroupAllR r1 = aibProcessGroupT r1 AibProcessGroup

aibProcessGroupAnyR :: (MonadCatch m) 
    => Rewrite c m AibProcessGroupKid -> Rewrite c m AibProcessGroup
aibProcessGroupAnyR r1 = unwrapAnyR $ aibProcessGroupAllR (wrapAnyR r1) 

aibProcessGroupOneR :: (MonadCatch m) 
    => Rewrite c m AibProcessGroupKid -> Rewrite c m AibProcessGroup
aibProcessGroupOneR r1 = unwrapOneR $ aibProcessGroupAllR (wrapOneR r1) 


-- ProcgKidProcess
aibProcessGroupKid_ProcessT :: (MonadThrow m) 
    => Transform c m AibProcess a1 
    -> (a1 -> b) 
    -> Transform c m AibProcessGroupKid b
aibProcessGroupKid_ProcessT t f = transform $ \c -> \case
    AibProcessGroupKid_Process obj -> 
        f <$> applyT t c obj   

        
aibProcessGroupKid_ProcessAllR :: (MonadThrow m) 
    => Rewrite c m AibProcess -> Rewrite c m AibProcessGroupKid
aibProcessGroupKid_ProcessAllR r1 = 
    aibProcessGroupKid_ProcessT r1 AibProcessGroupKid_Process

aibProcessGroupKid_ProcessAnyR :: (MonadCatch m) 
    => Rewrite c m AibProcess -> Rewrite c m AibProcessGroupKid
aibProcessGroupKid_ProcessAnyR r1 = 
    unwrapAnyR $ aibProcessGroupKid_ProcessAllR (wrapAnyR r1) 

aibProcessGroupKid_ProcessOneR :: (MonadCatch m) 
    => Rewrite c m AibProcess -> Rewrite c m AibProcessGroupKid
aibProcessGroupKid_ProcessOneR r1 = 
    unwrapOneR $ aibProcessGroupKid_ProcessAllR (wrapOneR r1)   



aibProcessT :: (MonadThrow m) 
    => Transform c m AibProcessKid a1 
    -> (String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m AibProcess b
aibProcessT t f = transform $ \c -> \case
    AibProcess ref name attrs kids -> 
        f ref name attrs <$> mapM (\x -> applyT t c x) kids   

        
aibProcessAllR :: (MonadThrow m) 
    => Rewrite c m AibProcessKid -> Rewrite c m AibProcess
aibProcessAllR r1 = aibProcessT r1 AibProcess

aibProcessAnyR :: (MonadCatch m) 
    => Rewrite c m AibProcessKid -> Rewrite c m AibProcess
aibProcessAnyR r1 = unwrapAnyR $ aibProcessAllR (wrapAnyR r1) 

aibProcessOneR :: (MonadCatch m) 
    => Rewrite c m AibProcessKid -> Rewrite c m AibProcess
aibProcessOneR r1 = unwrapOneR $ aibProcessAllR (wrapOneR r1)    


aibProcessKid_PlantAssemblyT :: (MonadThrow m) 
    => Transform c m AibPlantAssembly a1 
    -> (a1 -> b) 
    -> Transform c m AibProcessKid b
aibProcessKid_PlantAssemblyT t f = transform $ \c -> \case
    AibProcessKid_PlantAssembly obj -> 
        f <$> applyT t c obj   
    _   -> throwM (nodeMismatch "AibProcessKid_PlantAssembly") 
        
aibProcessKid_PlantAssemblyAllR :: (MonadThrow m) 
    => Rewrite c m AibPlantAssembly -> Rewrite c m AibProcessKid
aibProcessKid_PlantAssemblyAllR r1 = 
    aibProcessKid_PlantAssemblyT r1 AibProcessKid_PlantAssembly

aibProcessKid_PlantAssemblyAnyR :: (MonadCatch m) 
    => Rewrite c m AibPlantAssembly -> Rewrite c m AibProcessKid
aibProcessKid_PlantAssemblyAnyR r1 = 
    unwrapAnyR $ aibProcessKid_PlantAssemblyAllR (wrapAnyR r1) 

aibProcessKid_PlantAssemblyOneR :: (MonadCatch m) 
    => Rewrite c m AibPlantAssembly -> Rewrite c m AibProcessKid
aibProcessKid_PlantAssemblyOneR r1 = 
    unwrapOneR $ aibProcessKid_PlantAssemblyAllR (wrapOneR r1)   


aibProcessKid_PlantItemT :: (MonadThrow m) 
    => Transform c m AibPlantItem a1 
    -> (a1 -> b) 
    -> Transform c m AibProcessKid b
aibProcessKid_PlantItemT t f = transform $ \c -> \case
    AibProcessKid_PlantItem obj -> 
        f <$> applyT t c obj   
    _   -> throwM (nodeMismatch "AibProcessKid_PlantItem") 
        
aibProcessKid_PlantItemAllR :: (MonadThrow m) 
    => Rewrite c m AibPlantItem -> Rewrite c m AibProcessKid
aibProcessKid_PlantItemAllR r1 = 
    aibProcessKid_PlantItemT r1 AibProcessKid_PlantItem

aibProcessKid_PlantItemAnyR :: (MonadCatch m) 
    => Rewrite c m AibPlantItem-> Rewrite c m AibProcessKid
aibProcessKid_PlantItemAnyR r1 = 
    unwrapAnyR $ aibProcessKid_PlantItemAllR (wrapAnyR r1) 

aibProcessKid_PlantItemOneR :: (MonadCatch m) 
    => Rewrite c m AibPlantItem-> Rewrite c m AibProcessKid
aibProcessKid_PlantItemOneR r1 = 
    unwrapOneR $ aibProcessKid_PlantItemAllR (wrapOneR r1) 


-- AibPlantAssembly

aibPlantAssemblyT :: (MonadThrow m) 
    => Transform c m AibPlantAssemblyKid a1 
    -> (String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m AibPlantAssembly b
aibPlantAssemblyT t f = transform $ \c -> \case
    AibPlantAssembly ref name attrs kids -> 
        f ref name attrs <$> mapM (\x -> applyT t c x) kids  

        
aibPlantAssemblyAllR :: (MonadThrow m) 
    => Rewrite c m AibPlantAssemblyKid -> Rewrite c m AibPlantAssembly
aibPlantAssemblyAllR r1 = aibPlantAssemblyT r1 AibPlantAssembly

aibPlantAssemblyAnyR :: (MonadCatch m) 
    => Rewrite c m AibPlantAssemblyKid -> Rewrite c m AibPlantAssembly
aibPlantAssemblyAnyR r1 = unwrapAnyR $ aibPlantAssemblyAllR (wrapAnyR r1) 

aibPlantAssemblyOneR :: (MonadCatch m) 
    => Rewrite c m AibPlantAssemblyKid -> Rewrite c m AibPlantAssembly
aibPlantAssemblyOneR r1 = unwrapOneR $ aibPlantAssemblyAllR (wrapOneR r1)    


-- AibPlantAssemblyKid_PlantItem


aibPlantAssemblyKid_PlantItemT :: (MonadThrow m) 
    => Transform c m AibPlantItem a1 
    -> (a1 -> b) 
    -> Transform c m AibPlantAssemblyKid b
aibPlantAssemblyKid_PlantItemT t f = transform $ \c -> \case
    AibPlantAssemblyKid_PlantItem obj -> 
        f <$> applyT t c obj   
    _   -> throwM (nodeMismatch "AibPlantAssemblyKid_PlantItem")   
        
aibPlantAssemblyKid_PlantItemAllR :: (MonadThrow m) 
    => Rewrite c m AibPlantItem -> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_PlantItemAllR r1 = 
    aibPlantAssemblyKid_PlantItemT r1 AibPlantAssemblyKid_PlantItem

aibPlantAssemblyKid_PlantItemAnyR :: (MonadCatch m) 
    => Rewrite c m AibPlantItem -> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_PlantItemAnyR r1 = 
    unwrapAnyR $ aibPlantAssemblyKid_PlantItemAllR (wrapAnyR r1) 

aibPlantAssemblyKid_PlantItemOneR :: (MonadCatch m) 
    => Rewrite c m AibPlantItem-> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_PlantItemOneR r1 = 
    unwrapOneR $ aibPlantAssemblyKid_PlantItemAllR (wrapOneR r1) 

-- AibPlantAssemblyKid_Equipment

aibPlantAssemblyKid_EquipmentT :: (MonadThrow m) 
    => Transform c m AibEquipment a1 
    -> (a1 -> b) 
    -> Transform c m AibPlantAssemblyKid b
aibPlantAssemblyKid_EquipmentT t f = transform $ \c -> \case
    AibPlantAssemblyKid_Equipment obj -> 
        f <$> applyT t c obj   
    _   -> throwM (nodeMismatch "AibPlantAssemblyKid_Equipment")        
        
aibPlantAssemblyKid_EquipmentAllR :: (MonadThrow m) 
    => Rewrite c m AibEquipment -> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_EquipmentAllR r1 = 
    aibPlantAssemblyKid_EquipmentT r1 AibPlantAssemblyKid_Equipment

aibPlantAssemblyKid_EquipmentAnyR :: (MonadCatch m) 
    => Rewrite c m AibEquipment -> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_EquipmentAnyR r1 = 
    unwrapAnyR $ aibPlantAssemblyKid_EquipmentAllR (wrapAnyR r1) 

aibPlantAssemblyKid_EquipmentOneR :: (MonadCatch m) 
    => Rewrite c m AibEquipment -> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_EquipmentOneR r1 = 
    unwrapOneR $ aibPlantAssemblyKid_EquipmentAllR (wrapOneR r1) 
  

-- AibPlantAssemblyKid_Unknown    
    
aibPlantAssemblyKid_UnknownT :: (MonadThrow m) 
    => Transform c m AibUnknown a1 
    -> (a1 -> b) 
    -> Transform c m AibPlantAssemblyKid b
aibPlantAssemblyKid_UnknownT t f = transform $ \c -> \case
    AibPlantAssemblyKid_Unknown obj -> 
        f <$> applyT t c obj   
    _           -> throwM (nodeMismatch "AibPlantAssemblyKid_Unknown")
    
aibPlantAssemblyKid_UnknownAllR :: (MonadThrow m) 
    => Rewrite c m AibUnknown -> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_UnknownAllR r1 = 
    aibPlantAssemblyKid_UnknownT r1 AibPlantAssemblyKid_Unknown

aibPlantAssemblyKid_UnknownAnyR :: (MonadCatch m) 
    => Rewrite c m AibUnknown -> Rewrite c m AibPlantAssemblyKid
aibPlantAssemblyKid_UnknownAnyR r1 = 
    unwrapAnyR $ aibPlantAssemblyKid_UnknownAllR (wrapAnyR r1) 

aibPlantAssemblyKid_UnknownOneR :: (MonadCatch m) 
    => Rewrite c m AibUnknown -> Rewrite c m AibPlantAssemblyKid  
aibPlantAssemblyKid_UnknownOneR r1 = 
    unwrapOneR $ aibPlantAssemblyKid_UnknownAllR (wrapOneR r1) 



-- AibPlantItem

aibPlantItemT :: (MonadThrow m) 
    => Transform c m AibEquipment a1 
    -> (String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m AibPlantItem b
aibPlantItemT t f = transform $ \c -> \case
    AibPlantItem ref name attrs kids -> 
        f ref name attrs <$> mapM (\x -> applyT t c x) kids  

        
aibPlantItemAllR :: (MonadThrow m) 
    => Rewrite c m AibEquipment -> Rewrite c m AibPlantItem
aibPlantItemAllR r1 = aibPlantItemT r1 AibPlantItem

aibPlantItemAnyR :: (MonadCatch m) 
    => Rewrite c m AibEquipment -> Rewrite c m AibPlantItem
aibPlantItemAnyR r1 = unwrapAnyR $ aibPlantItemAllR (wrapAnyR r1) 

aibPlantItemOneR :: (MonadCatch m) 
    => Rewrite c m AibEquipment -> Rewrite c m AibPlantItem
aibPlantItemOneR r1 = unwrapOneR $ aibPlantItemAllR (wrapOneR r1)  


-- AibEquipment

aibEquipmentT :: (MonadThrow m) 
    => (String -> String -> Attributes -> b) 
    -> Transform c m AibEquipment b
aibEquipmentT f = transform $ \_ -> \case
    AibEquipment ref name attrs -> 
        pure (f ref name attrs)

-- AibEquipment

aibUnknownT :: (MonadThrow m) 
    => (String -> String -> Attributes -> b) 
    -> Transform c m AibUnknown b
aibUnknownT f = transform $ \_ -> \case
    AibUnknown ref name attrs -> 
        pure (f ref name attrs)






-------------------------------------------------------------------------------
-- KURE walker

instance Walker c AUniverse where
    allR :: MonadCatch m => Rewrite c m AUniverse -> Rewrite c m AUniverse
    allR r = 
            modExc (stackStrategyFailure "allR") $
            rewrite $ \ c -> \case
                AUInstallation v -> AUInstallation <$> applyR allR_Installation c v
                AUInstallationKid v -> AUInstallationKid <$> applyR allR_InstallationKid c v
                AUProcessGroup v -> AUProcessGroup <$> applyR allR_ProcessGroup c v
                AUProcessGroupKid v -> AUProcessGroupKid <$> applyR allR_ProcessGroupKid c v
                AUProcess v -> AUProcess <$> applyR allR_Process c v
                AUProcessKid v -> AUProcessKid <$> applyR allR_ProcessKid c v
                AUPlantAssembly v -> AUPlantAssembly <$> applyR allR_PlantAssembly c v
                AUPlantAssemblyKid v -> AUPlantAssemblyKid <$> applyR allR_PlantAssemblyKid c v
                AUPlantItem v -> AUPlantItem <$> applyR allR_PlantItem c v
                AUEquipment v -> AUEquipment <$> pure v
                AUUnknown v -> AUUnknown <$> pure v
        where
            allR_Installation           = aibInstallationAllR (extractR r)

            allR_InstallationKid        = 
                aibInstallationKid_ProcessGroupAllR (extractR r) 
                    <+ aibInstallationKid_ProcessAllR (extractR r)

            allR_ProcessGroup           = aibProcessGroupAllR (extractR r)

            allR_ProcessGroupKid        = 
                aibProcessGroupKid_ProcessAllR (extractR r) 

            allR_Process                = aibProcessAllR (extractR r)

            allR_ProcessKid             = 
                aibProcessKid_PlantAssemblyAllR (extractR r) 
                    <+ aibProcessKid_PlantItemAllR (extractR r)
                                
            allR_PlantAssembly          =  aibPlantAssemblyAllR (extractR r)   
            
            allR_PlantAssemblyKid       = 
                aibPlantAssemblyKid_PlantItemAllR (extractR r) 
                    <+ aibPlantAssemblyKid_EquipmentAllR (extractR r)   
                    <+ aibPlantAssemblyKid_UnknownAllR (extractR r)      
            
            allR_PlantItem              =  aibPlantItemAllR (extractR r) 
            

