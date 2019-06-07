{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.FlocLabelling
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- S4 pass to generate FLOC paths.
--
--------------------------------------------------------------------------------



module Assets.FlocLabelling 

    where

import Control.Exception.Base           

import Language.KURE                    -- package: KURE

import Assets.FlocPath
import Assets.S4Types
import Assets.S4Universe
import Assets.TranslateMonad

type TransformFlocLabel  a b = Transform FlocPath TranslateM a b
type RewriteFlocLabel    a   = TransformFlocLabel a a

applyRewrite :: RulesEnv -> RewriteFlocLabel a -> a -> Either String a
applyRewrite env r = runTranslateM env displayException . applyR r mempty

flocLabel :: RulesEnv -> S4Site -> Either String S4Site
flocLabel env site = 
    case project <$> applyRewrite env labelAll (inject site) of
        Left err -> Left err
        Right Nothing -> Left "flocLabel failed"
        Right (Just a) -> Right a


labelAll :: RewriteFlocLabel SUniverse
labelAll = 
    alltdR ( promoteR labelSite 
                <+ promoteR labelFunction
                <+ promoteR labelProcessGroup
                <+ promoteR labelProcess 
                <+ promoteR labelSystem
                <+ promoteR labelSubsystem
                <+ promoteR labelMainItem 
                <+ promoteR labelComponent )



labelSite :: RewriteFlocLabel S4Site
labelSite = idR

labelFunction :: RewriteFlocLabel S4Function
labelFunction = do
    item@S4Function {} <- idR
    floc <- toString <$> contextT
    return $ item { function_floc = floc }

    
labelProcessGroup :: RewriteFlocLabel S4ProcessGroup
labelProcessGroup = do
    item@S4ProcessGroup {} <- idR
    floc <- toString <$> contextT
    return $ item { process_group_floc = floc }

    
labelProcess :: RewriteFlocLabel S4Process
labelProcess = do
    item@S4Process {} <- idR
    floc <- toString <$> contextT
    return $ item { process_floc = floc }    
    
labelSystem :: RewriteFlocLabel S4System
labelSystem = do
    item@S4System {} <- idR
    floc <- toString <$> contextT
    return $ item { system_floc = floc }    

labelSubsystem :: RewriteFlocLabel S4Subsystem
labelSubsystem = do
    item@S4Subsystem {} <- idR
    floc <- toString <$> contextT
    return $ item { subsystem_floc = floc }       

labelMainItem :: RewriteFlocLabel S4MainItem
labelMainItem = do
    item@S4MainItem {} <- idR
    floc <- toString <$> contextT
    return $ item { main_item_floc = floc }       


labelComponent :: RewriteFlocLabel S4Component
labelComponent = do
    item@S4Component {} <- idR
    floc <- toString <$> contextT
    return $ item { component_floc = floc }       