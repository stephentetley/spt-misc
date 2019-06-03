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
    , process_item_code             :: String
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


instance JSON S4Component where
    showJSON :: S4Component -> JSValue
    showJSON a@(S4Component {}) = JSObject $ toJSObject $
        [ ("attributes",            showJSON (component_attributes a))
        , ("flocCode",              showJSON $ component_floc_code a)
        , ("itemCode",              showJSON $ component_code a)
        , ("kids",                  JSArray [])
        , ("name",                  showJSON $ component_name a)
        , ("type",                  showJSON "Equipment")
        ]

    readJSON :: JSValue -> Result (S4Component)
    readJSON (JSObject obj) = do 
        attrs     <- valFromObj "attributes"  obj >>= readJSON
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

data Universe = 
      U1Site          S4Site
    | U1Function      S4Function
    | U1ProcessGroup  S4ProcessGroup
    | U1Process       S4Process
    | U1System        S4System
    | U1Subsystem     S4Subsystem
    | U1MainItem      S4MainItem
    | U1Component     S4Component
    
---------------------------------------------------------------------------

instance Injection S4Site Universe where
  inject = U1Site

  project (U1Site v) = Just v
  project _ = Nothing
  

instance Injection S4Function Universe where
  inject = U1Function

  project (U1Function v) = Just v
  project _ = Nothing
  
  
instance Injection S4ProcessGroup Universe where
  inject = U1ProcessGroup

  project (U1ProcessGroup v) = Just v
  project _ = Nothing
  
  
instance Injection S4Process Universe where
  inject = U1Process

  project (U1Process v) = Just v
  project _ = Nothing  

  
instance Injection S4System Universe where
  inject = U1System

  project (U1System v) = Just v
  project _ = Nothing    

  
instance Injection S4Subsystem Universe where
  inject = U1Subsystem

  project (U1Subsystem v) = Just v
  project _ = Nothing  
  
instance Injection S4MainItem Universe where
  inject = U1MainItem

  project (U1MainItem v) = Just v
  project _ = Nothing 


instance Injection S4Component Universe where
  inject = U1Component

  project (U1Component v) = Just v
  project _ = Nothing   