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


module Assets.TranslationRules where

import Control.Monad.Reader.Class
import qualified Data.Map as Map

import Language.KURE.MonadCatch         -- package: KURE
import Text.JSON                        -- package: json

import Assets.Common

data FlocInfo = FlocInfo 
    { site_type             :: String
    , s4_name               :: String
    , level1_code           :: String
    , level2_code           :: String
    }
    deriving (Eq, Ord, Show)

type SaiNumber = String 

type FlocMapping  = Map.Map SaiNumber FlocInfo

readFlocMapping :: JSValue -> Result FlocMapping
readFlocMapping (JSArray xs) =
    Map.fromList <$> mapM readPair xs

readFlocMapping _ = Error "Not a JSArray" 
    

readPair :: JSValue -> Result (SaiNumber, FlocInfo)
readPair (JSObject obj) = do 
    (,) <$> readField "sai"        readJSONString  obj
        <*> readField  "flocInfo"  readFlocInfo    obj

readPair _ = Error "Not a JSObject"  

readFlocInfo :: JSValue -> Result FlocInfo
readFlocInfo (JSObject obj) = do 
    FlocInfo    <$> readField "type"        readJSONString obj
                <*> readField "s4Name"      readJSONString obj
                <*> readField "level1Code"  readJSONString obj
                <*> readField "level2Code"  readJSONString obj

readFlocInfo _ = Error "Not a JSObject" 

readField :: String -> (JSValue -> Result a) -> JSObject JSValue -> Result a
readField name parser obj = valFromObj name obj >>= parser

readJSONString :: JSValue -> Result String
readJSONString = readJSON

class HasFlocMapping env where
    getFlocMapping :: env -> FlocMapping

flocMappingInfo :: (HasFlocMapping env, MonadReader env m, MonadThrow m)
    => SaiNumber -> m FlocInfo 
flocMappingInfo sai = do
    dict <- asks getFlocMapping
    case Map.lookup sai dict of
        Nothing -> throwM (LookupException $ "no Site Mapping: " ++ sai)
        Just ans -> return ans

