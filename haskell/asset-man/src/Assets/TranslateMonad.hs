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
        Env(..)
    ,   loadRules
    ,   TranslateM
    ,   runTranslateM
    ,   siteFlocMappingInfo
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

data Env = Env 
    {   floc_mapping :: SiteFlocMapping
    }

loadRules :: FilePath -> IO Env
loadRules flocMappingPath = do
    floc_map <- readSiteFlocMapping flocMappingPath
    return Env { floc_mapping = floc_map }



newtype TranslateM a = TranslateM { getTranslateM :: Env -> Result a }

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


instance MonadReader Env TranslateM where
    ask = TranslateM $ \env -> Right env
    local f ma = TranslateM $ \env -> getTranslateM ma (f env)


-- | Eliminator for 'runTranslateM'.
runTranslateM :: Env -> (SomeException -> String) -> TranslateM a -> Either String a
runTranslateM env fk ma = 
    case getTranslateM ma env of
        Left exc -> Left (fk exc)
        Right ans -> Right ans




siteFlocMappingInfo :: SaiNumber -> TranslateM SiteFlocInfo 
siteFlocMappingInfo sai = do
    dict <- asks floc_mapping
    case siteFlocMappingLookup sai dict of
        Nothing -> throwM (LookupException $ "no Site Mapping: " ++ sai)
        Just ans -> return ans
