{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.TranslateMonad
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- TranslateMonad
--
--------------------------------------------------------------------------------


module Assets.TranslateMonad
    (
        RulesEnv(..)
    ,   RulesConfig(..)
    ,   loadRules
    ,   TranslateM
    ,   runTranslateM
    ,   getSiteFlocInfo
    ,   getProcessFlocInfo

    ,   level2FunctionDescription
    ,   level3ProcessGroupDescription
    ,   level4ProcessDescription
    ) where

import Prelude hiding ( fail )
import Control.Exception        
import Control.Monad.Fail
import Control.Monad.Reader.Class
import qualified Data.Map as Map

import Language.KURE.MonadCatch         -- package: KURE

import Assets.Common
import Assets.TranslationRules

data TranslateException = TranslateException String
    deriving (Eq, Show)

instance Exception TranslateException where
    displayException :: TranslateException -> String
    displayException (TranslateException msg) = "TranslateException: " ++ msg


type Result a = Either SomeException a

data RulesEnv = RulesEnv 
    {   floc_1_2_mapping        :: SiteFlocMapping
    ,   floc_2_3_mapping        :: ProcessGroupFlocMapping
    ,   floc_2_3_4_mapping      :: ProcessFlocMapping
    ,   level_2_descriptions    :: StringDictionary
    ,   level_3_descriptions    :: StringDictionary
    ,   level_4_descriptions    :: StringDictionary
    }

data RulesConfig = RulesConfig
    { path_to_levels_1_2_mapping_file       :: String
    , path_to_levels_2_3_mapping_file       :: String
    , path_to_levels_2_3_4_mapping_file     :: String
    , path_to_level_2_descriptions          :: String
    , path_to_level_3_descriptions          :: String
    , path_to_level_4_descriptions          :: String
    }

loadRules :: RulesConfig -> IO RulesEnv
loadRules config = do
    site_floc_map       <- readSiteFlocMapping (path_to_levels_1_2_mapping_file config)
    proc_group_floc_map <- readProcessGroupFlocMapping (path_to_levels_2_3_mapping_file config)
    proc_floc_map       <- readProcessFlocMapping (path_to_levels_2_3_4_mapping_file config)
    lev2_descrs         <- readStringDictionary (path_to_level_2_descriptions config)
    lev3_descrs         <- readStringDictionary (path_to_level_3_descriptions config)
    lev4_descrs         <- readStringDictionary (path_to_level_4_descriptions config)
    return $ RulesEnv  
                { floc_1_2_mapping          = site_floc_map
                , floc_2_3_4_mapping        = proc_floc_map 
                , floc_2_3_mapping          = proc_group_floc_map
                , level_2_descriptions      = lev2_descrs 
                , level_3_descriptions      = lev3_descrs 
                , level_4_descriptions      = lev4_descrs
                }



newtype TranslateM a = TranslateM { getTranslateM :: RulesEnv -> Result a }

instance Functor TranslateM where
    fmap f ma = TranslateM $ \env -> f <$> getTranslateM ma env

instance Applicative TranslateM where
    pure a = TranslateM $ \_ -> pure a
    mf <*> ma = TranslateM $ \env -> getTranslateM mf env <*> getTranslateM ma env

instance Monad TranslateM where
    ma >>= k = TranslateM $ \env -> 
                    getTranslateM ma env >>= \ans -> getTranslateM (k ans) env

instance MonadFail TranslateM where
    fail :: String -> TranslateM a
    fail msg = TranslateM $ \_ -> Left $ SomeException $ TranslateException msg 

instance MonadThrow TranslateM where
    throwM :: Exception e => e -> TranslateM a
    throwM exc = TranslateM $ \_ -> Left $ toException exc

instance MonadCatch TranslateM where
    catch :: Exception exc => TranslateM a -> (exc -> TranslateM a) -> TranslateM a    
    catch ma f = TranslateM $ \env -> 
                    case getTranslateM ma env of
                        Left e1 -> 
                            case fromException e1 of
                                Just ex -> getTranslateM (f ex) env
                                Nothing -> Left e1
                        Right ans -> Right ans


instance MonadReader RulesEnv TranslateM where
    ask = TranslateM $ \env -> Right env
    local f ma = TranslateM $ \env -> getTranslateM ma (f env)


-- | Eliminator for 'runTranslateM'.
runTranslateM :: RulesEnv -> (SomeException -> String) -> TranslateM a -> Either String a
runTranslateM env fk ma = 
    case getTranslateM ma env of
        Left exc -> Left (fk exc)
        Right ans -> Right ans




getSiteFlocInfo :: SaiNumber -> TranslateM SiteFlocInfo 
getSiteFlocInfo sai = do
    dict <- asks floc_1_2_mapping
    case siteFlocMappingLookup sai dict of
        Nothing -> throwM (LookupException $ "no Site Mapping: " ++ sai)
        Just ans -> return ans


getProcessGroupFlocInfo :: String -> String -> TranslateM (String, String) 
getProcessGroupFlocInfo instType groupName = do
    let aibKey = ProcessGroupAibKey 
                    { key23_inst_type                 = instType
                    , key23_proc_group_description    = groupName
                    }

    dict <- asks floc_2_3_mapping
    case processGroupFlocMappingLookup aibKey dict of
        Nothing -> throwM (LookupException $ "no Process Group Mapping: " ++ show aibKey)
        Just ans -> return ( info23_level2_code ans, info23_level3_code ans )

getProcessFlocInfo :: String -> String -> String -> TranslateM (String, String, String) 
getProcessFlocInfo instType groupName procName = do
    let aibKey = ProcessAibKey 
                    { key234_inst_type                 = instType
                    , key234_proc_group_description    = groupName
                    , key234_proc_description          = procName
                    }

    dict <- asks floc_2_3_4_mapping
    case processFlocMappingLookup aibKey dict of
        Nothing -> throwM (LookupException $ "no Process Mapping: " ++ show aibKey)
        Just ans -> return ( info234_level2_code ans, info234_level3_code ans, info234_level4_code ans )


level2FunctionDescription :: String -> TranslateM String 
level2FunctionDescription code = do
    dict <- asks level_2_descriptions
    case stringLookup code dict of
        Nothing -> return $ "{" ++ code ++ "}"
        Just ans -> return ans       


level3ProcessGroupDescription :: String -> TranslateM String 
level3ProcessGroupDescription code = do
    dict <- asks level_3_descriptions
    case stringLookup code dict of
        Nothing -> return $ "{" ++ code ++ "}"
        Just ans -> return ans   
                
level4ProcessDescription :: String -> TranslateM String 
level4ProcessDescription code = do
    dict <- asks level_4_descriptions
    case stringLookup code dict of
        Nothing -> return $ "{" ++ code ++ "}"
        Just ans -> return ans           