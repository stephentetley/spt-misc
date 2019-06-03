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
-- Module      :  Assets.S4Types
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- S4 Types.
--
--------------------------------------------------------------------------------



module Assets.S4Types
  ( 
    S4Site(..)
  , S4Function(..)
  , S4ProcessGroup(..)
  , S4Process(..)
  , S4System(..)
  , S4Subsystem(..)
  , S4MainItem(..)
  , S4Component(..)

  , SUniverse(..)

  , s4SiteT
  , s4SiteAllR
  , s4SiteAnyR
  , s4SiteOneR

  , s4FunctionT
  , s4FunctionAllR
  , s4FunctionAnyR
  , s4FunctionOneR

  , s4ProcessGroupT
  , s4ProcessGroupAllR
  , s4ProcessGroupAnyR
  , s4ProcessGroupOneR

  , s4ProcessT
  , s4ProcessAllR
  , s4ProcessAnyR
  , s4ProcessOneR

  , s4SystemT
  , s4SystemAllR
  , s4SystemAnyR
  , s4SystemOneR

  
  , s4SubsystemT
  , s4SubsystemAllR
  , s4SubsystemAnyR
  , s4SubsystemOneR

  
  , s4MainItemT
  , s4MainItemAllR
  , s4MainItemAnyR
  , s4MainItemOneR

  ) where

import Text.JSON                        -- package: json
import Language.KURE                    -- package: kure

import Assets.Common


  
data S4Site = S4Site
    { site_code           :: String
    , site_name           :: String
    , site_attributes     :: Attributes
    , site_kids           :: [S4Function]
    }
    deriving (Eq,Ord,Show)

  
data S4Function = S4Function
    { function_floc_code      :: String
    , function_code           :: String
    , function_name           :: String
    , function_attributes     :: Attributes
    , function_kids           :: [S4ProcessGroup]
    }
    deriving (Eq,Ord,Show)
  
data S4ProcessGroup = S4ProcessGroup
    { process_group_floc_code       :: String
    , process_group_code            :: String
    , process_group_name            :: String
    , process_group_attributes      :: Attributes
    , process_group_kids            :: [S4Process]
    }
    deriving (Eq,Ord,Show)
    
      
data S4Process = S4Process
    { process_floc_code             :: String
    , process_code                  :: String
    , process_name                  :: String
    , process_attributes            :: Attributes
    , process_kids                  :: [S4System]
    }
    deriving (Eq,Ord,Show)

  
data S4System = S4System
    { system_floc_code     :: String
    , system_code     :: String
    , system_name          :: String
    , system_attributes    :: Attributes
    , system_kids          :: [S4Subsystem]
    }
    deriving (Eq,Ord,Show)
    
      
data S4Subsystem = S4Subsystem
    { subsystem_floc_code     :: String
    , subsystem_code     :: String
    , subsystem_name          :: String
    , subsystem_attributes    :: Attributes
    , subsystem_kids          :: [S4MainItem]
    }
    deriving (Eq,Ord,Show)

data S4MainItem = S4MainItem
    { main_item_floc_code     :: String
    , main_item_code     :: String
    , main_item_name          :: String
    , main_item_attributes    :: Attributes
    , main_item_kids          :: [S4Component]
    }
    deriving (Eq,Ord,Show)

data S4Component = S4Component
    { component_floc_code     :: String
    , component_code          :: String
    , component_name          :: String
    , component_attributes    :: Attributes
    }
    deriving (Eq,Ord,Show)

-------------------------------------------------------------------------------
-- JSON


instance JSON S4Site where
  showJSON :: S4Site -> JSValue
  showJSON a@(S4Site {}) = JSObject $ toJSObject $
      [ ("attributes",            attributesToJSON $ site_attributes a)
      , ("itemCode",              showJSON $ site_code a)
      , ("flocCode",              showJSON $ site_code a)
      , ("kids",                  JSArray (map showJSON $ site_kids a))
      , ("name",                  showJSON $ site_name a)
      , ("type",                  showJSON "Site")
      ]

  readJSON :: JSValue -> Result (S4Site)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      code      <- valFromObj "itemCode"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "Site" then 
          return (S4Site { site_attributes    = attrs
                         , site_code          = code 
                         , site_kids          = kids
                         , site_name          = name
                         })
      else
          fail "Not a site"

  readJSON _ = Error "Not a site"

  
instance JSON S4Function where
  showJSON :: S4Function -> JSValue
  showJSON a@(S4Function {}) = JSObject $ toJSObject $
      [ ("attributes",            attributesToJSON $ function_attributes a)
      , ("flocCode",              showJSON $ function_floc_code a)
      , ("itemCode",              showJSON $ function_code a)
      , ("kids",                  JSArray (map showJSON $ function_kids a))
      , ("name",                  showJSON $ function_name a)
      , ("type",                  showJSON "Function")
      ]

  readJSON :: JSValue -> Result (S4Function)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      flocCode  <- valFromObj "flocCode"    obj >>= readJSON
      itemCode  <- valFromObj "itemCode"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "Function" then 
          return (S4Function { function_floc_code     = flocCode 
                             , function_code          = itemCode
                             , function_kids          = kids
                             , function_name          = name
                             , function_attributes    = attrs
                             })
      else
          fail "Not a function"

  readJSON _ = Error "Not a function"

  
instance JSON S4ProcessGroup where
  showJSON :: S4ProcessGroup -> JSValue
  showJSON a@(S4ProcessGroup {}) = JSObject $ toJSObject $
      [ ("attributes",            attributesToJSON $ process_group_attributes a)
      , ("flocCode",              showJSON $ process_group_floc_code a)
      , ("itemCode",              showJSON $ process_group_code a)
      , ("kids",                  JSArray (map showJSON $ process_group_kids a))
      , ("name",                  showJSON $ process_group_name a)
      , ("type",                  showJSON "ProcessGroup")
      ]

  readJSON :: JSValue -> Result (S4ProcessGroup)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      flocCode  <- valFromObj "flocCode"    obj >>= readJSON
      itemCode  <- valFromObj "itemCode"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "ProcessGroup" then 
          return (S4ProcessGroup { process_group_floc_code     = flocCode 
                                  , process_group_code          = itemCode
                                  , process_group_kids          = kids
                                  , process_group_name          = name
                                  , process_group_attributes    = attrs
                                  })
      else
          fail "Not a process group"

  readJSON _ = Error "Not a process group"

      
instance JSON S4Process where
  showJSON :: S4Process -> JSValue
  showJSON a@(S4Process {}) = JSObject $ toJSObject $
      [ ("attributes",            attributesToJSON $ process_attributes a)
      , ("flocCode",              showJSON $ process_floc_code a)
      , ("itemCode",              showJSON $ process_code a)
      , ("kids",                  JSArray (map showJSON $ process_kids a))
      , ("name",                  showJSON $ process_name a)
      , ("type",                  showJSON "Process")
      ]
  
  readJSON :: JSValue -> Result (S4Process)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      flocCode  <- valFromObj "flocCode"    obj >>= readJSON
      itemCode  <- valFromObj "itemCode"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "Process" then 
          return (S4Process { process_floc_code     = flocCode 
                            , process_code          = itemCode
                            , process_kids          = kids
                            , process_name          = name
                            , process_attributes    = attrs
                            })
      else
          fail "Not a process"

  readJSON _ = Error "Not a process"

instance JSON S4System where
  showJSON :: S4System -> JSValue
  showJSON a@(S4System {}) = JSObject $ toJSObject $
      [ ("attributes",            attributesToJSON $ system_attributes a)
      , ("flocCode",              showJSON $ system_floc_code a)
      , ("itemCode",              showJSON $ system_code a)
      , ("kids",                  JSArray (map showJSON $ system_kids a))
      , ("name",                  showJSON $ system_name a)
      , ("type",                  showJSON "MainItem")
      ]

  readJSON :: JSValue -> Result (S4System)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      flocCode  <- valFromObj "flocCode"    obj >>= readJSON
      itemCode  <- valFromObj "itemCode"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "MainItem" then 
          return (S4System { system_floc_code     = flocCode 
                           , system_code          = itemCode
                           , system_kids          = kids
                           , system_name          = name
                           , system_attributes    = attrs
                           })
      else
          fail "Not a system"

  readJSON _ = Error "Not a system"

instance JSON S4Subsystem where
  showJSON :: S4Subsystem -> JSValue
  showJSON a@(S4Subsystem {}) = JSObject $ toJSObject $
      [ ("attributes",            attributesToJSON $ subsystem_attributes a)
      , ("flocCode",              showJSON $ subsystem_floc_code a)
      , ("itemCode",              showJSON $ subsystem_code a)
      , ("kids",                  JSArray (map showJSON $ subsystem_kids a))
      , ("name",                  showJSON $ subsystem_name a)
      , ("type",                  showJSON "MainItem")
      ]

  readJSON :: JSValue -> Result (S4Subsystem)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      flocCode  <- valFromObj "flocCode"    obj >>= readJSON
      itemCode  <- valFromObj "itemCode"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "MainItem" then 
          return (S4Subsystem { subsystem_floc_code     = flocCode 
                              , subsystem_code          = itemCode
                              , subsystem_kids          = kids
                              , subsystem_name          = name
                              , subsystem_attributes    = attrs
                              })
      else
          fail "Not a subsystem"

  readJSON _ = Error "Not a subsystem"


instance JSON S4MainItem where
  showJSON :: S4MainItem -> JSValue
  showJSON a@(S4MainItem {}) = JSObject $ toJSObject $
      [ ("attributes",            attributesToJSON $ main_item_attributes a)
      , ("flocCode",              showJSON $ main_item_floc_code a)
      , ("itemCode",              showJSON $ main_item_code a)
      , ("kids",                  JSArray (map showJSON $ main_item_kids a))
      , ("name",                  showJSON $ main_item_name a)
      , ("type",                  showJSON "MainItem")
      ]

  readJSON :: JSValue -> Result (S4MainItem)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      flocCode  <- valFromObj "flocCode"    obj >>= readJSON
      itemCode  <- valFromObj "itemCode"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "MainItem" then 
          return (S4MainItem { main_item_floc_code     = flocCode 
                             , main_item_code          = itemCode
                             , main_item_kids          = kids
                             , main_item_name          = name
                             , main_item_attributes    = attrs
                             } )
      else
          fail "Not a main item"

  readJSON _ = Error "Not a main item"


instance JSON S4Component where
    showJSON :: S4Component -> JSValue
    showJSON a@(S4Component {}) = JSObject $ toJSObject $
        [ ("attributes",            attributesToJSON $ component_attributes a)
        , ("flocCode",              showJSON $ component_floc_code a)
        , ("itemCode",              showJSON $ component_code a)
        , ("kids",                  JSArray [])
        , ("name",                  showJSON $ component_name a)
        , ("type",                  showJSON "Equipment")
        ]

    readJSON :: JSValue -> Result (S4Component)
    readJSON (JSObject obj) = do 
        attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
        flocCode  <- valFromObj "flocCode"    obj >>= readJSON
        itemCode  <- valFromObj "itemCode"    obj >>= readJSON
        name      <- valFromObj "name"        obj >>= readJSON
        itemtype  <- valFromObj "type"        obj >>= readJSON
        if itemtype == "Equipment" then 
            return (S4Component { component_floc_code     = flocCode 
                                , component_code          = itemCode
                                , component_name          = name
                                , component_attributes    = attrs
                                } )
        else
            fail "Not a component"

    readJSON _ = Error "Not a component"

-------------------------------------------------------------------------------
-- KURE

data SUniverse = 
      SUSite          S4Site
    | SUFunction      S4Function
    | SUProcessGroup  S4ProcessGroup
    | SUProcess       S4Process
    | SUSystem        S4System
    | SUSubsystem     S4Subsystem
    | SUMainItem      S4MainItem
    | SUComponent     S4Component
    
---------------------------------------------------------------------------

instance Injection S4Site SUniverse where
  inject = SUSite

  project (SUSite v) = Just v
  project _ = Nothing
  

instance Injection S4Function SUniverse where
  inject = SUFunction

  project (SUFunction v) = Just v
  project _ = Nothing
  
  
instance Injection S4ProcessGroup SUniverse where
  inject = SUProcessGroup

  project (SUProcessGroup v) = Just v
  project _ = Nothing
  
  
instance Injection S4Process SUniverse where
  inject = SUProcess

  project (SUProcess v) = Just v
  project _ = Nothing  

  
instance Injection S4System SUniverse where
  inject = SUSystem

  project (SUSystem v) = Just v
  project _ = Nothing    

  
instance Injection S4Subsystem SUniverse where
  inject = SUSubsystem

  project (SUSubsystem v) = Just v
  project _ = Nothing  
  
instance Injection S4MainItem SUniverse where
  inject = SUMainItem

  project (SUMainItem v) = Just v
  project _ = Nothing 


instance Injection S4Component SUniverse where
  inject = SUComponent

  project (SUComponent v) = Just v
  project _ = Nothing 
  

-------------------------------------------------------------------------------
-- KURE congruence combinators


-- S4Site

s4SiteT :: (MonadThrow m) 
    => Transform c m S4Function a1 
    -> (String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Site b
s4SiteT t f = transform $ \c -> \case
    S4Site code name attrs kids -> 
        f code name attrs <$> mapM (\x -> applyT t c x) kids    

s4SiteAllR :: (MonadThrow m) 
    => Rewrite c m S4Function -> Rewrite c m S4Site
s4SiteAllR r1 = s4SiteT r1 S4Site

s4SiteAnyR :: (MonadCatch m) 
    => Rewrite c m S4Function -> Rewrite c m S4Site
s4SiteAnyR r1 = unwrapAnyR $ s4SiteAllR (wrapAnyR r1) 

s4SiteOneR :: (MonadCatch m) 
    => Rewrite c m S4Function -> Rewrite c m S4Site
s4SiteOneR r1 = unwrapOneR $ s4SiteAllR (wrapOneR r1) 

-- S4Function

s4FunctionT :: (MonadThrow m) 
    => Transform c m S4ProcessGroup a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Function b
s4FunctionT t f = transform $ \c -> \case
    S4Function floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4FunctionAllR :: (MonadThrow m) 
    => Rewrite c m S4ProcessGroup -> Rewrite c m S4Function
s4FunctionAllR r1 = s4FunctionT r1 S4Function

s4FunctionAnyR :: (MonadCatch m) 
    => Rewrite c m S4ProcessGroup -> Rewrite c m S4Function
s4FunctionAnyR r1 = unwrapAnyR $ s4FunctionAllR (wrapAnyR r1) 

s4FunctionOneR :: (MonadCatch m) 
    => Rewrite c m S4ProcessGroup -> Rewrite c m S4Function
s4FunctionOneR r1 = unwrapOneR $ s4FunctionAllR (wrapOneR r1) 


-- S4ProcessGroup

s4ProcessGroupT :: (MonadThrow m) 
    => Transform c m S4Process a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4ProcessGroup b
s4ProcessGroupT t f = transform $ \c -> \case
    S4ProcessGroup floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4ProcessGroupAllR :: (MonadThrow m) 
    => Rewrite c m S4Process -> Rewrite c m S4ProcessGroup
s4ProcessGroupAllR r1 = s4ProcessGroupT r1 S4ProcessGroup

s4ProcessGroupAnyR :: (MonadCatch m) 
    => Rewrite c m S4Process -> Rewrite c m S4ProcessGroup
s4ProcessGroupAnyR r1 = unwrapAnyR $ s4ProcessGroupAllR (wrapAnyR r1) 

s4ProcessGroupOneR :: (MonadCatch m) 
    => Rewrite c m S4Process -> Rewrite c m S4ProcessGroup
s4ProcessGroupOneR r1 = unwrapOneR $ s4ProcessGroupAllR (wrapOneR r1) 

-- S4Process

s4ProcessT :: (MonadThrow m) 
    => Transform c m S4System a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Process b
s4ProcessT t f = transform $ \c -> \case
    S4Process floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4ProcessAllR :: (MonadThrow m) 
    => Rewrite c m S4System -> Rewrite c m S4Process
s4ProcessAllR r1 = s4ProcessT r1 S4Process

s4ProcessAnyR :: (MonadCatch m) 
    => Rewrite c m S4System -> Rewrite c m S4Process
s4ProcessAnyR r1 = unwrapAnyR $ s4ProcessAllR (wrapAnyR r1) 

s4ProcessOneR :: (MonadCatch m) 
    => Rewrite c m S4System -> Rewrite c m S4Process
s4ProcessOneR r1 = unwrapOneR $ s4ProcessAllR (wrapOneR r1)


-- S4System

s4SystemT :: (MonadThrow m) 
    => Transform c m S4Subsystem a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4System b
s4SystemT t f = transform $ \c -> \case
    S4System floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4SystemAllR :: (MonadThrow m) 
    => Rewrite c m S4Subsystem -> Rewrite c m S4System
s4SystemAllR r1 = s4SystemT r1 S4System

s4SystemAnyR :: (MonadCatch m) 
    => Rewrite c m S4Subsystem -> Rewrite c m S4System
s4SystemAnyR r1 = unwrapAnyR $ s4SystemAllR (wrapAnyR r1) 

s4SystemOneR :: (MonadCatch m) 
    => Rewrite c m S4Subsystem -> Rewrite c m S4System
s4SystemOneR r1 = unwrapOneR $ s4SystemAllR (wrapOneR r1)

-- S4Subsystem

s4SubsystemT :: (MonadThrow m) 
    => Transform c m S4MainItem a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Subsystem b
s4SubsystemT t f = transform $ \c -> \case
    S4Subsystem floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4SubsystemAllR :: (MonadThrow m) 
    => Rewrite c m S4MainItem -> Rewrite c m S4Subsystem
s4SubsystemAllR r1 = s4SubsystemT r1 S4Subsystem

s4SubsystemAnyR :: (MonadCatch m) 
    => Rewrite c m S4MainItem -> Rewrite c m S4Subsystem
s4SubsystemAnyR r1 = unwrapAnyR $ s4SubsystemAllR (wrapAnyR r1) 

s4SubsystemOneR :: (MonadCatch m) 
    => Rewrite c m S4MainItem -> Rewrite c m S4Subsystem
s4SubsystemOneR r1 = unwrapOneR $ s4SubsystemAllR (wrapOneR r1)


-- S4MainItem

s4MainItemT :: (MonadThrow m) 
    => Transform c m S4Component a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4MainItem b
s4MainItemT t f = transform $ \c -> \case
    S4MainItem floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4MainItemAllR :: (MonadThrow m) 
    => Rewrite c m S4Component -> Rewrite c m S4MainItem
s4MainItemAllR r1 = s4MainItemT r1 S4MainItem

s4MainItemAnyR :: (MonadCatch m) 
    => Rewrite c m S4Component -> Rewrite c m S4MainItem
s4MainItemAnyR r1 = unwrapAnyR $ s4MainItemAllR (wrapAnyR r1) 

s4MainItemOneR :: (MonadCatch m) 
    => Rewrite c m S4Component -> Rewrite c m S4MainItem
s4MainItemOneR r1 = unwrapOneR $ s4MainItemAllR (wrapOneR r1)

-------------------------------------------------------------------------------
-- KURE walker

instance Walker c SUniverse where
    allR :: MonadCatch m => Rewrite c m SUniverse -> Rewrite c m SUniverse
    allR r = 
            modExc (stackStrategyFailure "allR") $
            rewrite $ \ c -> \case
                SUSite v -> SUSite <$> applyR allR_Site c v
                SUFunction v -> SUFunction <$> applyR allR_Function c v
                SUProcessGroup v -> SUProcessGroup <$> applyR allR_ProcessGroup c v
                SUProcess v -> SUProcess <$> applyR allR_Process c v
                SUSystem v -> SUSystem <$> applyR allR_System c v
                SUSubsystem v -> SUSubsystem <$> applyR allR_Subsystem c v
                SUMainItem v -> SUMainItem <$> applyR allR_MainItem c v
                SUComponent v -> SUComponent <$> pure v
        where
            allR_Site           = s4SiteAllR (extractR r)
            allR_Function       = s4FunctionAllR (extractR r)
            allR_ProcessGroup   = s4ProcessGroupAllR (extractR r)
            allR_Process        = s4ProcessAllR (extractR r)
            allR_System         = s4SystemAllR (extractR r)
            allR_Subsystem      = s4SubsystemAllR (extractR r)
            allR_MainItem       = s4MainItemAllR (extractR r)