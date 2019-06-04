{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Assets.Facts.SiteNameMapping
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  to be determined
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Map installation (Aib) to site (S4)
--
--------------------------------------------------------------------------------

module Assets.Facts.SiteNameMapping
    (
        SiteDetails(..)
    , siteNameMapping
    ) where

import Prelude hiding ( fail )        
import Control.Monad.Fail                   -- package: base

data SiteDetails = SiteDetails 
    { site_type     :: String
    , site_name     :: String
    , floc1         :: String
    , floc2         :: String
    }
    deriving (Eq, Ord, Show)

-- "SAI00001000" ("ALDERBROOKS/STW") -> 'STW', 'Alderbrooks WwTW', 'ALD01', 'WWT').


-- This is just a placeholder, the real data is operational / private

siteNameMapping :: MonadFail m =>  String -> m SiteDetails
siteNameMapping "SAI00001000"   = return $ SiteDetails "STW" "Alderbrooks WwTW" "ALD01" "WWT"
siteNameMapping key             = fail ("no Site Mapping: " ++ key)