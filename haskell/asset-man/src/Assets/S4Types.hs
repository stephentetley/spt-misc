{-# LANGUAGE DoAndIfThenElse            #-}
{-# LANGUAGE InstanceSigs               #-}
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

 
  , coalesceFunctions

  ) where

import Data.List

import Text.JSON                        -- package: json

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
-- Coalescing (when building) 

coalesceFunctions :: [S4Function] -> [S4Function]
coalesceFunctions = 
        map coalesce1 . groupBy equality . sortBy ordering
    where
        ordering a b = compare (function_floc_code a) (function_floc_code b)
        equality a b = function_floc_code a == function_floc_code b
        coalesce1 [] = error "coalesceFunctions - impossible"
        coalesce1 (x:xs) = foldr coalesceB x xs
        coalesceB a acc = 
            let kids1 = function_kids acc ++ function_kids a
            in acc { function_kids = kids1 }

       