{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.S4Universe
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- S4 Universe for KURE.
--
--------------------------------------------------------------------------------



module Assets.S4Universe
  ( 
    SUniverse(..)

  , s4SiteT
  , s4SiteAllR
  , s4SiteAnyR
  , s4SiteOneR

  , s4FunctionT
  , s4FunctionAllR
  , s4FunctionAnyR
  , s4FunctionOneR

  , s4ProcessGroupT
  , s4ProcessGroupAllR
  , s4ProcessGroupAnyR
  , s4ProcessGroupOneR

  , s4ProcessT
  , s4ProcessAllR
  , s4ProcessAnyR
  , s4ProcessOneR

  , s4SystemT
  , s4SystemAllR
  , s4SystemAnyR
  , s4SystemOneR

  
  , s4SubsystemT
  , s4SubsystemAllR
  , s4SubsystemAnyR
  , s4SubsystemOneR

  
  , s4MainItemT
  , s4MainItemAllR
  , s4MainItemAnyR
  , s4MainItemOneR
  
  ) where


import Language.KURE                    -- package: kure

import Assets.Common
import Assets.S4Types


-------------------------------------------------------------------------------
-- KURE

data SUniverse = 
      SUSite          S4Site
    | SUFunction      S4Function
    | SUProcessGroup  S4ProcessGroup
    | SUProcess       S4Process
    | SUSystem        S4System
    | SUSubsystem     S4Subsystem
    | SUMainItem      S4MainItem
    | SUComponent     S4Component
    
---------------------------------------------------------------------------

instance Injection S4Site SUniverse where
  inject = SUSite

  project (SUSite v) = Just v
  project _ = Nothing
  

instance Injection S4Function SUniverse where
  inject = SUFunction

  project (SUFunction v) = Just v
  project _ = Nothing
  
  
instance Injection S4ProcessGroup SUniverse where
  inject = SUProcessGroup

  project (SUProcessGroup v) = Just v
  project _ = Nothing
  
  
instance Injection S4Process SUniverse where
  inject = SUProcess

  project (SUProcess v) = Just v
  project _ = Nothing  

  
instance Injection S4System SUniverse where
  inject = SUSystem

  project (SUSystem v) = Just v
  project _ = Nothing    

  
instance Injection S4Subsystem SUniverse where
  inject = SUSubsystem

  project (SUSubsystem v) = Just v
  project _ = Nothing  
  
instance Injection S4MainItem SUniverse where
  inject = SUMainItem

  project (SUMainItem v) = Just v
  project _ = Nothing 


instance Injection S4Component SUniverse where
  inject = SUComponent

  project (SUComponent v) = Just v
  project _ = Nothing 
  

-------------------------------------------------------------------------------
-- KURE congruence combinators


-- S4Site

s4SiteT :: (MonadThrow m) 
    => Transform c m S4Function a1 
    -> (String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Site b
s4SiteT t f = transform $ \c -> \case
    S4Site code name attrs kids -> 
        f code name attrs <$> mapM (\x -> applyT t c x) kids    

s4SiteAllR :: (MonadThrow m) 
    => Rewrite c m S4Function -> Rewrite c m S4Site
s4SiteAllR r1 = s4SiteT r1 S4Site

s4SiteAnyR :: (MonadCatch m) 
    => Rewrite c m S4Function -> Rewrite c m S4Site
s4SiteAnyR r1 = unwrapAnyR $ s4SiteAllR (wrapAnyR r1) 

s4SiteOneR :: (MonadCatch m) 
    => Rewrite c m S4Function -> Rewrite c m S4Site
s4SiteOneR r1 = unwrapOneR $ s4SiteAllR (wrapOneR r1) 

-- S4Function

s4FunctionT :: (MonadThrow m) 
    => Transform c m S4ProcessGroup a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Function b
s4FunctionT t f = transform $ \c -> \case
    S4Function floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4FunctionAllR :: (MonadThrow m) 
    => Rewrite c m S4ProcessGroup -> Rewrite c m S4Function
s4FunctionAllR r1 = s4FunctionT r1 S4Function

s4FunctionAnyR :: (MonadCatch m) 
    => Rewrite c m S4ProcessGroup -> Rewrite c m S4Function
s4FunctionAnyR r1 = unwrapAnyR $ s4FunctionAllR (wrapAnyR r1) 

s4FunctionOneR :: (MonadCatch m) 
    => Rewrite c m S4ProcessGroup -> Rewrite c m S4Function
s4FunctionOneR r1 = unwrapOneR $ s4FunctionAllR (wrapOneR r1) 


-- S4ProcessGroup

s4ProcessGroupT :: (MonadThrow m) 
    => Transform c m S4Process a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4ProcessGroup b
s4ProcessGroupT t f = transform $ \c -> \case
    S4ProcessGroup floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4ProcessGroupAllR :: (MonadThrow m) 
    => Rewrite c m S4Process -> Rewrite c m S4ProcessGroup
s4ProcessGroupAllR r1 = s4ProcessGroupT r1 S4ProcessGroup

s4ProcessGroupAnyR :: (MonadCatch m) 
    => Rewrite c m S4Process -> Rewrite c m S4ProcessGroup
s4ProcessGroupAnyR r1 = unwrapAnyR $ s4ProcessGroupAllR (wrapAnyR r1) 

s4ProcessGroupOneR :: (MonadCatch m) 
    => Rewrite c m S4Process -> Rewrite c m S4ProcessGroup
s4ProcessGroupOneR r1 = unwrapOneR $ s4ProcessGroupAllR (wrapOneR r1) 

-- S4Process

s4ProcessT :: (MonadThrow m) 
    => Transform c m S4System a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Process b
s4ProcessT t f = transform $ \c -> \case
    S4Process floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4ProcessAllR :: (MonadThrow m) 
    => Rewrite c m S4System -> Rewrite c m S4Process
s4ProcessAllR r1 = s4ProcessT r1 S4Process

s4ProcessAnyR :: (MonadCatch m) 
    => Rewrite c m S4System -> Rewrite c m S4Process
s4ProcessAnyR r1 = unwrapAnyR $ s4ProcessAllR (wrapAnyR r1) 

s4ProcessOneR :: (MonadCatch m) 
    => Rewrite c m S4System -> Rewrite c m S4Process
s4ProcessOneR r1 = unwrapOneR $ s4ProcessAllR (wrapOneR r1)


-- S4System

s4SystemT :: (MonadThrow m) 
    => Transform c m S4Subsystem a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4System b
s4SystemT t f = transform $ \c -> \case
    S4System floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4SystemAllR :: (MonadThrow m) 
    => Rewrite c m S4Subsystem -> Rewrite c m S4System
s4SystemAllR r1 = s4SystemT r1 S4System

s4SystemAnyR :: (MonadCatch m) 
    => Rewrite c m S4Subsystem -> Rewrite c m S4System
s4SystemAnyR r1 = unwrapAnyR $ s4SystemAllR (wrapAnyR r1) 

s4SystemOneR :: (MonadCatch m) 
    => Rewrite c m S4Subsystem -> Rewrite c m S4System
s4SystemOneR r1 = unwrapOneR $ s4SystemAllR (wrapOneR r1)

-- S4Subsystem

s4SubsystemT :: (MonadThrow m) 
    => Transform c m S4MainItem a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4Subsystem b
s4SubsystemT t f = transform $ \c -> \case
    S4Subsystem floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4SubsystemAllR :: (MonadThrow m) 
    => Rewrite c m S4MainItem -> Rewrite c m S4Subsystem
s4SubsystemAllR r1 = s4SubsystemT r1 S4Subsystem

s4SubsystemAnyR :: (MonadCatch m) 
    => Rewrite c m S4MainItem -> Rewrite c m S4Subsystem
s4SubsystemAnyR r1 = unwrapAnyR $ s4SubsystemAllR (wrapAnyR r1) 

s4SubsystemOneR :: (MonadCatch m) 
    => Rewrite c m S4MainItem -> Rewrite c m S4Subsystem
s4SubsystemOneR r1 = unwrapOneR $ s4SubsystemAllR (wrapOneR r1)


-- S4MainItem

s4MainItemT :: (MonadThrow m) 
    => Transform c m S4Component a1 
    -> (String -> String -> String -> Attributes -> [a1] -> b) 
    -> Transform c m S4MainItem b
s4MainItemT t f = transform $ \c -> \case
    S4MainItem floc code name attrs kids -> 
        f floc code name attrs <$> mapM (\x -> applyT t c x) kids    

s4MainItemAllR :: (MonadThrow m) 
    => Rewrite c m S4Component -> Rewrite c m S4MainItem
s4MainItemAllR r1 = s4MainItemT r1 S4MainItem

s4MainItemAnyR :: (MonadCatch m) 
    => Rewrite c m S4Component -> Rewrite c m S4MainItem
s4MainItemAnyR r1 = unwrapAnyR $ s4MainItemAllR (wrapAnyR r1) 

s4MainItemOneR :: (MonadCatch m) 
    => Rewrite c m S4Component -> Rewrite c m S4MainItem
s4MainItemOneR r1 = unwrapOneR $ s4MainItemAllR (wrapOneR r1)

-------------------------------------------------------------------------------
-- KURE walker

instance Walker c SUniverse where
    allR :: MonadCatch m => Rewrite c m SUniverse -> Rewrite c m SUniverse
    allR r = 
            modExc (stackStrategyFailure "allR") $
            rewrite $ \ c -> \case
                SUSite v -> SUSite <$> applyR allR_Site c v
                SUFunction v -> SUFunction <$> applyR allR_Function c v
                SUProcessGroup v -> SUProcessGroup <$> applyR allR_ProcessGroup c v
                SUProcess v -> SUProcess <$> applyR allR_Process c v
                SUSystem v -> SUSystem <$> applyR allR_System c v
                SUSubsystem v -> SUSubsystem <$> applyR allR_Subsystem c v
                SUMainItem v -> SUMainItem <$> applyR allR_MainItem c v
                SUComponent v -> SUComponent <$> pure v
        where
            allR_Site           = s4SiteAllR (extractR r)
            allR_Function       = s4FunctionAllR (extractR r)
            allR_ProcessGroup   = s4ProcessGroupAllR (extractR r)
            allR_Process        = s4ProcessAllR (extractR r)
            allR_System         = s4SystemAllR (extractR r)
            allR_Subsystem      = s4SubsystemAllR (extractR r)
            allR_MainItem       = s4MainItemAllR (extractR r)



       