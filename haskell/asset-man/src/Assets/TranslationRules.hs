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
    ,   SiteFlocMapping
    ,   SiteFlocInfo(..)
    ,   readSiteFlocMapping
    ,   siteFlocMappingLookup

    ,   ProcessFlocMapping
    ,   ProcessAibKey(..)
    ,   ProcessFlocInfo(..)
    ,   readProcessFlocMapping
    ,   processFlocMappingLookup

    ) where


import qualified Data.Map as Map

import Text.JSON                        -- package: json


type SaiNumber = String 

readMapping :: JSON a => FilePath -> IO a
readMapping path = do
    json <- readFile path
    case decode json of
        Ok a -> return a
        Error msg -> error msg

--------------------------------------------------------------------------------
-- Sites (L1, L2)

newtype SiteFlocMapping  = SiteFlocMapping 
    { siteMapping :: Map.Map SaiNumber SiteFlocInfo }

data SiteFlocInfo = SiteFlocInfo 
    { site_type             :: !String
    , site_s4_name          :: !String
    , site_level1_code      :: !String
    , site_level2_code      :: !String
    }
    deriving (Eq, Ord, Show)



readSiteFlocMapping :: FilePath -> IO SiteFlocMapping
readSiteFlocMapping path = readMapping path

siteFlocMappingLookup :: SaiNumber -> SiteFlocMapping -> Maybe SiteFlocInfo
siteFlocMappingLookup sai dict = Map.lookup sai (siteMapping dict)


instance JSON SiteFlocMapping where
    readJSON :: JSValue -> Result SiteFlocMapping
    readJSON (JSArray xs) =
        (SiteFlocMapping . Map.fromList) <$> mapM readSiteFlocMapping1 xs

    readJSON _ = Error "Not a JSArray" 

    showJSON _ = error "showJSON not supported"
    

readSiteFlocMapping1 :: JSValue -> Result (SaiNumber, SiteFlocInfo)
readSiteFlocMapping1 (JSObject obj) = 
    (,) <$> readField "sai"        readJSONString       obj
        <*> readField  "flocInfo"  readSiteFlocInfo     obj

readSiteFlocMapping1 _ = Error "Not a JSObject"  

readSiteFlocInfo :: JSValue -> Result SiteFlocInfo
readSiteFlocInfo (JSObject obj) = 
    SiteFlocInfo    <$> readField "type"        readJSONString obj
                    <*> readField "s4Name"      readJSONString obj
                    <*> readField "level1Code"  readJSONString obj
                    <*> readField "level2Code"  readJSONString obj

readSiteFlocInfo _ = Error "Not a JSObject" 

readField :: String -> (JSValue -> Result a) -> JSObject JSValue -> Result a
readField name parser obj = valFromObj name obj >>= parser

readJSONString :: JSValue -> Result String
readJSONString = readJSON

--------------------------------------------------------------------------------
-- Processes (L2, L3, L4)

newtype ProcessFlocMapping  = ProcessFlocMapping 
    { procMapping :: Map.Map ProcessAibKey ProcessFlocInfo }


data ProcessAibKey = ProcessAibKey 
    { inst_type                 :: !String
    , proc_group_description    :: !String
    , proc_description          :: !String
    }
    deriving (Eq, Ord, Show)

data ProcessFlocInfo = ProcessFlocInfo 
    { proc_level2_code          :: !String
    , proc_level3_code          :: !String
    , proc_level4_code          :: !String
    }
    deriving (Eq, Ord, Show)



readProcessFlocMapping :: FilePath -> IO ProcessFlocMapping
readProcessFlocMapping path = readMapping path    


processFlocMappingLookup :: ProcessAibKey -> ProcessFlocMapping -> Maybe ProcessFlocInfo
processFlocMappingLookup aibKey dict = Map.lookup aibKey (procMapping dict)


instance JSON ProcessFlocMapping where
    readJSON :: JSValue -> Result ProcessFlocMapping
    readJSON (JSArray xs) =
        (ProcessFlocMapping . Map.fromList) <$> mapM readProcessMapping1 xs

    readJSON _ = Error "Not a JSArray" 

    showJSON _ = error "showJSON not supported"

readProcessMapping1 :: JSValue -> Result (ProcessAibKey, ProcessFlocInfo) 
readProcessMapping1 (JSObject obj) =
    (,) <$> readField "aib"     readProcessAibKey       obj
        <*> readField "s4"      readProcessFlocInfo     obj

readProcessMapping1 _ = Error "Not a JSObject"  

readProcessAibKey :: JSValue -> Result ProcessAibKey
readProcessAibKey (JSObject obj) = 
    ProcessAibKey   <$> readField "instType"        readJSONString obj
                    <*> readField "progGroupDesc"   readJSONString obj
                    <*> readField "procDesc"        readJSONString obj

readProcessAibKey _ = Error "Not a JSObject" 

readProcessFlocInfo :: JSValue -> Result ProcessFlocInfo
readProcessFlocInfo (JSObject obj) = 
    ProcessFlocInfo <$> readField "level2Floc"  readJSONString obj
                    <*> readField "level3Floc"  readJSONString obj
                    <*> readField "level4Floc"  readJSONString obj
                    

readProcessFlocInfo _ = Error "Not a JSObject" 