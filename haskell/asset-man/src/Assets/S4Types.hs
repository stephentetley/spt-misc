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

 
  , mergeFunctions

  ) where


import Data.Function ( on )
import Data.List

import Text.JSON                        -- package: json

import Assets.Common


  
data S4Site = S4Site
    { site_floc                 :: String
    , site_name                 :: String
    , site_attributes           :: Attributes
    , site_kids                 :: [S4Function]
    }
    deriving (Eq,Ord,Show)

  
data S4Function = S4Function
    { function_floc             :: String
    , function_name             :: String
    , function_attributes       :: Attributes
    , function_kids             :: [S4ProcessGroup]
    }
    deriving (Eq,Ord,Show)
  
data S4ProcessGroup = S4ProcessGroup
    { process_group_floc        :: String
    , process_group_name        :: String
    , process_group_attributes  :: Attributes
    , process_group_kids        :: [S4Process]
    }
    deriving (Eq,Ord,Show)
    
      
data S4Process = S4Process
    { process_floc              :: String
    , process_name              :: String
    , process_attributes        :: Attributes
    , process_kids              :: [S4System]
    }
    deriving (Eq,Ord,Show)

  
data S4System = S4System
    { system_floc               :: String
    , system_name               :: String
    , system_attributes         :: Attributes
    , system_kids               :: [S4Subsystem]
    }
    deriving (Eq,Ord,Show)
    
      
data S4Subsystem = S4Subsystem
    { subsystem_floc            :: String
    , subsystem_name            :: String
    , subsystem_attributes      :: Attributes
    , subsystem_kids            :: [S4MainItem]
    }
    deriving (Eq,Ord,Show)

data S4MainItem = S4MainItem
    { main_item_floc            :: String
    , main_item_name            :: String
    , main_item_attributes      :: Attributes
    , main_item_kids            :: [S4Component]
    }
    deriving (Eq,Ord,Show)

data S4Component = S4Component
    { component_floc            :: String
    , component_name            :: String
    , component_attributes      :: Attributes
    }
    deriving (Eq,Ord,Show)

-------------------------------------------------------------------------------
-- JSON


instance JSON S4Site where
  showJSON :: S4Site -> JSValue
  showJSON a@(S4Site {}) = JSObject $ toJSObject $
      [ ("attributes",        attributesToJSON $ site_attributes a)
      , ("floc",              showJSON $ site_floc a)
      , ("kids",              JSArray (map showJSON $ site_kids a))
      , ("name",              showJSON $ site_name a)
      , ("type",              showJSON "Site")
      ]

  readJSON :: JSValue -> Result (S4Site)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      floc      <- valFromObj "floc"        obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "Site" then 
          return (S4Site { site_attributes    = attrs
                         , site_floc          = floc 
                         , site_kids          = kids
                         , site_name          = name
                         })
      else
          fail "Not a site"

  readJSON _ = Error "Not a site"

  
instance JSON S4Function where
  showJSON :: S4Function -> JSValue
  showJSON a@(S4Function {}) = JSObject $ toJSObject $
      [ ("attributes",        attributesToJSON $ function_attributes a)
      , ("floc",              showJSON $ function_floc a)
      , ("kids",              JSArray (map showJSON $ function_kids a))
      , ("name",              showJSON $ function_name a)
      , ("type",              showJSON "Function")
      ]

  readJSON :: JSValue -> Result (S4Function)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      floc      <- valFromObj "floc"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "Function" then 
          return (S4Function { function_floc        = floc 
                             , function_kids        = kids
                             , function_name        = name
                             , function_attributes  = attrs
                             })
      else
          fail "Not a function"

  readJSON _ = Error "Not a function"

  
instance JSON S4ProcessGroup where
  showJSON :: S4ProcessGroup -> JSValue
  showJSON a@(S4ProcessGroup {}) = JSObject $ toJSObject $
      [ ("attributes",        attributesToJSON $ process_group_attributes a)
      , ("floc",              showJSON $ process_group_floc a)
      , ("kids",              JSArray (map showJSON $ process_group_kids a))
      , ("name",              showJSON $ process_group_name a)
      , ("type",              showJSON "ProcessGroup")
      ]

  readJSON :: JSValue -> Result (S4ProcessGroup)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      floc      <- valFromObj "floc"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "ProcessGroup" then 
          return (S4ProcessGroup { process_group_floc         = floc 
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
      [ ("attributes",          attributesToJSON $ process_attributes a)
      , ("floc",                showJSON $ process_floc a)
      , ("kids",                JSArray (map showJSON $ process_kids a))
      , ("name",                showJSON $ process_name a)
      , ("type",                showJSON "Process")
      ]
  
  readJSON :: JSValue -> Result (S4Process)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      floc      <- valFromObj "floc"    obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "Process" then 
          return (S4Process { process_floc          = floc 
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
      [ ("attributes",          attributesToJSON $ system_attributes a)
      , ("floc",                showJSON $ system_floc a)
      , ("kids",                JSArray (map showJSON $ system_kids a))
      , ("name",                showJSON $ system_name a)
      , ("type",                showJSON "MainItem")
      ]

  readJSON :: JSValue -> Result (S4System)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      floc      <- valFromObj "floc"        obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "MainItem" then 
          return (S4System { system_floc        = floc 
                           , system_kids        = kids
                           , system_name        = name
                           , system_attributes  = attrs
                           })
      else
          fail "Not a system"

  readJSON _ = Error "Not a system"

instance JSON S4Subsystem where
  showJSON :: S4Subsystem -> JSValue
  showJSON a@(S4Subsystem {}) = JSObject $ toJSObject $
      [ ("attributes",          attributesToJSON $ subsystem_attributes a)
      , ("floc",                showJSON $ subsystem_floc a)
      , ("kids",                JSArray (map showJSON $ subsystem_kids a))
      , ("name",                showJSON $ subsystem_name a)
      , ("type",                showJSON "MainItem")
      ]

  readJSON :: JSValue -> Result (S4Subsystem)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      floc      <- valFromObj "floc"        obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "MainItem" then 
          return (S4Subsystem { subsystem_floc          = floc 
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
      [ ("attributes",          attributesToJSON $ main_item_attributes a)
      , ("floc",                showJSON $ main_item_floc a)
      , ("kids",                JSArray (map showJSON $ main_item_kids a))
      , ("name",                showJSON $ main_item_name a)
      , ("type",                showJSON "MainItem")
      ]

  readJSON :: JSValue -> Result (S4MainItem)
  readJSON (JSObject obj) = do 
      attrs     <- valFromObj "attributes"  obj >>= attributesFromJSON
      floc      <- valFromObj "floc"        obj >>= readJSON
      kids      <- valFromObj "kids"        obj >>= \(JSArray xs) -> mapM readJSON xs
      name      <- valFromObj "name"        obj >>= readJSON
      itemtype  <- valFromObj "type"        obj >>= readJSON
      if itemtype == "MainItem" then 
          return (S4MainItem { main_item_floc       = floc
                             , main_item_kids       = kids
                             , main_item_name       = name
                             , main_item_attributes = attrs
                             } )
      else
          fail "Not a main item"

  readJSON _ = Error "Not a main item"


instance JSON S4Component where
    showJSON :: S4Component -> JSValue
    showJSON a@(S4Component {}) = JSObject $ toJSObject $
        [ ("attributes",            attributesToJSON $ component_attributes a)
        , ("floc",                  showJSON $ component_floc a)
        , ("kids",                  JSArray [])
        , ("name",                  showJSON $ component_name a)
        , ("type",                  showJSON "Equipment")
        ]

    readJSON :: JSValue -> Result (S4Component)
    readJSON (JSObject obj) = do 
        attrs     <- valFromObj "attributes"    obj >>= attributesFromJSON
        floc      <- valFromObj "floc"          obj >>= readJSON
        name      <- valFromObj "name"          obj >>= readJSON
        itemtype  <- valFromObj "type"          obj >>= readJSON
        if itemtype == "Equipment" then 
            return (S4Component { component_floc          = floc
                                , component_name          = name
                                , component_attributes    = attrs
                                } )
        else
            fail "Not a component"

    readJSON _ = Error "Not a component"


 
-------------------------------------------------------------------------------
-- Coalescing (when building) 

mergeFunctions :: [S4Function] -> [S4Function]
mergeFunctions = 
        map coalesce . groupBy equality . sortBy ordering
    where
        ordering = on compare function_floc
        equality = on (==) function_floc
        coalesce [] = error "mergeFunctions - impossible"
        coalesce (x:xs) = foldr coalesce2 x xs
        coalesce2 a acc = 
            let kids1 = mergeProcessGroups (function_kids acc ++ function_kids a)
            in acc { function_kids = kids1 }

mergeProcessGroups :: [S4ProcessGroup] -> [S4ProcessGroup]
mergeProcessGroups = 
        map coalesce . groupBy equality . sortBy ordering
    where
        ordering = on compare process_group_floc
        equality = on (==) process_group_floc
        coalesce [] = error "mergeProcessGroups - impossible"
        coalesce (x:xs) = foldr coalesce2 x xs
        coalesce2 a acc = 
            let kids1 = mergeProcesses (process_group_kids acc ++ process_group_kids a)
            in acc { process_group_kids = kids1 }

mergeProcesses :: [S4Process] -> [S4Process]
mergeProcesses = 
        map coalesce . groupBy equality . sortBy ordering
    where
        ordering = on compare process_floc
        equality = on (==) process_floc
        coalesce [] = error "mergeProcesses - impossible"
        coalesce (x:xs) = foldr coalesce2 x xs
        coalesce2 a acc = 
            let kids1 = mergeSystems (process_kids acc ++ process_kids a)
            in acc { process_kids = kids1 }


mergeSystems :: [S4System] -> [S4System]
mergeSystems = 
        map coalesce . groupBy equality . sortBy ordering
    where
        ordering = on compare system_floc
        equality = on (==) system_floc
        coalesce [] = error "mergeSystems - impossible"
        coalesce (x:xs) = foldr coalesce2 x xs
        coalesce2 a acc = 
            let kids1 = mergeSubsystems (system_kids acc ++ system_kids a)
            in acc { system_kids = kids1 }


mergeSubsystems :: [S4Subsystem] -> [S4Subsystem]
mergeSubsystems = 
        map coalesce . groupBy equality . sortBy ordering
    where
        ordering = on compare subsystem_floc
        equality = on (==) subsystem_floc
        coalesce [] = error "mergeSubsystems - impossible"
        coalesce (x:xs) = foldr coalesce2 x xs
        coalesce2 a acc = 
            let kids1 = mergeMainItems (subsystem_kids acc ++ subsystem_kids a)
            in acc { subsystem_kids = kids1 }


mergeMainItems :: [S4MainItem] -> [S4MainItem]
mergeMainItems = 
        map coalesce . groupBy equality . sortBy ordering
    where
        ordering = on compare main_item_floc
        equality = on (==) main_item_floc
        coalesce [] = error "mergeMainItems - impossible"
        coalesce (x:xs) = foldr coalesce2 x xs
        coalesce2 a acc = 
            let kids1 = main_item_kids acc ++ main_item_kids a
            in acc { main_item_kids = kids1 }

