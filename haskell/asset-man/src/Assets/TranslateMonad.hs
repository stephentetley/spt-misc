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
-- FlocPath
--
--------------------------------------------------------------------------------


module Assets.TranslateMonad
    (
        TranslateM
    ,   runTranslateM
    ) where

import Prelude hiding ( fail )
import Control.Exception        
import Control.Monad.Fail
import Control.Monad.Reader.Class

import Language.KURE.MonadCatch         -- package: KURE


data TranslateException = TranslateException String
    deriving (Eq, Show)

instance Exception TranslateException where
    displayException :: TranslateException -> String
    displayException (TranslateException msg) = "TranslateException: " ++ msg


type Result a = Either SomeException a

newtype TranslateM env a = TranslateM { getTranslateM :: env -> Result a }

instance Functor (TranslateM env) where
    fmap f ma = TranslateM $ \env -> f <$> getTranslateM ma env

instance Applicative (TranslateM env) where
    pure a = TranslateM $ \_ -> pure a
    mf <*> ma = TranslateM $ \env -> getTranslateM mf env <*> getTranslateM ma env

instance Monad (TranslateM env) where
    ma >>= k = TranslateM $ \env -> 
                    getTranslateM ma env >>= \ans -> getTranslateM (k ans) env

instance MonadFail (TranslateM env) where
    fail :: String -> TranslateM env a
    fail msg = TranslateM $ \_ -> Left $ SomeException $ TranslateException msg 

instance MonadThrow (TranslateM env) where
    throwM :: Exception e => e -> TranslateM env a
    throwM exc = TranslateM $ \_ -> Left $ toException exc

instance MonadCatch  (TranslateM env) where
    catch :: Exception exc => TranslateM env a -> (exc -> TranslateM env a) -> TranslateM env a    
    catch ma f = TranslateM $ \env -> 
                    case getTranslateM ma env of
                        Left e1 -> 
                            case fromException e1 of
                                Just ex -> getTranslateM (f ex) env
                                Nothing -> Left e1
                        Right ans -> Right ans


instance MonadReader env (TranslateM env) where
    ask = TranslateM $ \env -> Right env
    local f ma = TranslateM $ \env -> getTranslateM ma (f env)
    

-- | Eliminator for 'runTranslateM'.
runTranslateM :: env -> (SomeException -> String) -> TranslateM env a -> Either String a
runTranslateM env fk ma = 
    case getTranslateM ma env of
        Left exc -> Left (fk exc)
        Right ans -> Right ans
