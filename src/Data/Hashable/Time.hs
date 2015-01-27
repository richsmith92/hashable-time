{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hashable.Time () where

import Data.Fixed
import Data.Hashable (Hashable(..))
import Data.Time
import GHC.Generics  (Generic)


instance Hashable DiffTime where
  hashWithSalt s = hashWithSalt s . toRational

instance Hashable NominalDiffTime where
  hashWithSalt s = hashWithSalt s . toRational

deriving instance Generic (Fixed a)
instance Hashable (Fixed a)

deriving instance Generic Day
instance Hashable Day

deriving instance Generic TimeOfDay
instance Hashable TimeOfDay

deriving instance Generic UTCTime
instance Hashable UTCTime

deriving instance Generic UniversalTime
instance Hashable UniversalTime

deriving instance Generic TimeZone
instance Hashable TimeZone

deriving instance Generic LocalTime
instance Hashable LocalTime

deriving instance Generic ZonedTime
instance Hashable ZonedTime
