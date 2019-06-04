{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Asset.S4Pretty
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- S4 Pretty print.
--
--------------------------------------------------------------------------------



module Assets.S4Pretty 
    (
        s4DrawTree  
    ) where

import Data.Tree

import Language.KURE                    -- package: KURE

import Assets.S4Types
import Assets.S4Universe


demo01 = drawTree $ Node "one" [Node "two" [], Node "three" []]


type TransformE a b = Transform () KureM a b
type RewriteE a b = TransformE a b

applyTransform :: TransformE a b -> a -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t ()



s4DrawTree :: S4Site -> String
s4DrawTree site = 
    case applyTransform  siteTree site of
        Left err -> err
        Right tree -> drawTree tree


siteTree :: TransformE S4Site (Tree String)
siteTree = do
    site@S4Site {} <- idR
    kids <- s4SiteT functionTree (\_ _ _ -> id)
    return $ Node (site_name site) kids

functionTree ::  TransformE S4Function (Tree String)   
functionTree = do
    func@S4Function {} <- idR
    kids <- s4FunctionT processGroupTree (\_ _ _ _ -> id)
    return $ Node (function_name func) kids

processGroupTree ::  TransformE S4ProcessGroup (Tree String)   
processGroupTree = do
    procg@S4ProcessGroup {} <- idR
    kids <- return []
    return $ Node (process_group_name procg) kids    