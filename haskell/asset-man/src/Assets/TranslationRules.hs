{-# LANGUAGE InstanceSigs               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.TranslationRules
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Read translation rules encoded as JSON.
-- Put them into a data structure than can be used 
-- by the translation monad.
--
--------------------------------------------------------------------------------


module Assets.TranslationRules 
    (
        SaiNumber
    ,   SiteFlocInfo(..)
    ,   SiteFlocMapping
    ,   readSiteFlocMapping
    ,   siteFlocMappingLookup
    ) where


import qualified Data.Map as Map

import Language.KURE.MonadCatch         -- package: KURE
import Text.JSON                        -- package: json

import Assets.Common

type SaiNumber = String 


data SiteFlocInfo = SiteFlocInfo 
    { site_type             :: String
    , s4_name               :: String
    , level1_code           :: String
    , level2_code           :: String
    }
    deriving (Eq, Ord, Show)


newtype SiteFlocMapping  = SiteFlocMapping { flocMapping :: Map.Map SaiNumber SiteFlocInfo }

readSiteFlocMapping :: FilePath -> IO SiteFlocMapping
readSiteFlocMapping path = do
    json <- readFile path
    case decode json of
        Ok a -> return a
        Error msg -> error msg

siteFlocMappingLookup :: SaiNumber -> SiteFlocMapping -> Maybe SiteFlocInfo
siteFlocMappingLookup sai dict = Map.lookup sai (flocMapping dict)


instance JSON SiteFlocMapping where
    readJSON :: JSValue -> Result SiteFlocMapping
    readJSON (JSArray xs) =
        (SiteFlocMapping . Map.fromList) <$> mapM readSiteFlocPair xs

    readJSON _ = Error "Not a JSArray" 

    showJSON _ = error "showJSON not supported"
    

readSiteFlocPair :: JSValue -> Result (SaiNumber, SiteFlocInfo)
readSiteFlocPair (JSObject obj) = do 
    (,) <$> readField "sai"        readJSONString       obj
        <*> readField  "flocInfo"  readSiteFlocInfo     obj

readSitePair _ = Error "Not a JSObject"  

readSiteFlocInfo :: JSValue -> Result SiteFlocInfo
readSiteFlocInfo (JSObject obj) = do 
    SiteFlocInfo    <$> readField "type"        readJSONString obj
                    <*> readField "s4Name"      readJSONString obj
                    <*> readField "level1Code"  readJSONString obj
                    <*> readField "level2Code"  readJSONString obj

readFlocInfo _ = Error "Not a JSObject" 

readField :: String -> (JSValue -> Result a) -> JSObject JSValue -> Result a
readField name parser obj = valFromObj name obj >>= parser

readJSONString :: JSValue -> Result String
readJSONString = readJSON


