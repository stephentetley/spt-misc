{-# LANGUAGE InstanceSigs               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Asset.Common
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



module Assets.Common where

import Control.Exception.Base 
import qualified Data.Map as Map


import Text.JSON                            -- package: json

data LookupException = LookupException String
    deriving (Eq, Show)

instance Exception LookupException where
    displayException :: LookupException -> String
    displayException (LookupException msg) = "LookupException: " ++ msg




type Attributes = Map.Map String AttrValue

data AttrValue
    = AttrBool Bool
    | AttrString String
    deriving (Eq,Ord,Show)

noAttrs :: Attributes
noAttrs = Map.empty    

encodeAttrValue :: AttrValue -> JSValue
encodeAttrValue (AttrBool bool)     = JSBool bool
encodeAttrValue (AttrString str)    = JSString $ toJSString str



attributesToJSON :: Attributes -> JSValue
attributesToJSON attrs = 
    JSObject $ toJSObject $ map (\(k,v) -> (k, encodeAttrValue v)) $ Map.toAscList attrs

decodeAttrValue :: JSValue -> Result AttrValue
decodeAttrValue (JSBool bool)   = Ok (AttrBool bool)
decodeAttrValue (JSString str)  = Ok (AttrString $ fromJSString str)
decodeAttrValue _               = fail "unknown attr type"

decodeNameValue :: (String,JSValue) -> Result (String, AttrValue)
decodeNameValue (key,json) = decodeAttrValue json >>= \val -> return (key,val)


attributesFromJSON :: JSValue -> Result Attributes
attributesFromJSON (JSObject obj) = do
    xs <- mapM decodeNameValue (fromJSObject obj)
    return (Map.fromList xs)

attributesFromJSON _ = fail "attributesFromJSON - Not a JSObject"
