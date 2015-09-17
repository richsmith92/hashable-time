{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Hashable.Time
-- Description : Hashable instances for Data.Time types
-- License     : BSD3
-- Maintainer  : Alexey Karakulov <ankarakulov@gmail.com>
module Data.Hashable.Time (Hashable(..)) where

import Data.Fixed
import Data.Hashable (Hashable(..))
import Data.Time

#if !MIN_VERSION_time(1,5,0)
import System.Locale
#endif

-- Dependencies

-- ! (>=1.2.4) ~ <1.2.4 ~ <= 1.2.3
#if !MIN_VERSION_hashable(1,2,4)
-- https://github.com/tibbe/hashable/pull/101
instance Hashable (Fixed a) where
   hashWithSalt salt (MkFixed i) = hashWithSalt salt i
#endif

-- Data.Time.Clock

instance Hashable UniversalTime where
  hashWithSalt salt = hashWithSalt salt . getModJulianDate

instance Hashable DiffTime where
  hashWithSalt salt = hashWithSalt salt . toRational

instance Hashable UTCTime where
  hashWithSalt salt (UTCTime d dt) =
    salt `hashWithSalt` d `hashWithSalt` dt

instance Hashable NominalDiffTime where
  hashWithSalt salt = hashWithSalt salt . toRational

-- Data.Time.Calendar

instance Hashable Day where
  hashWithSalt salt (ModifiedJulianDay d) = hashWithSalt salt d

-- Data.Time.LocalTime

instance Hashable TimeZone where
  hashWithSalt salt (TimeZone m s n) =
    salt `hashWithSalt` m `hashWithSalt` s `hashWithSalt` n

instance Hashable TimeOfDay where
  hashWithSalt salt (TimeOfDay h m s) =
    salt `hashWithSalt` h `hashWithSalt` m `hashWithSalt` s

instance Hashable LocalTime where
  hashWithSalt salt (LocalTime d tod) =
    salt `hashWithSalt` d `hashWithSalt` tod

instance Hashable ZonedTime where
  hashWithSalt salt (ZonedTime lt tz) =
    salt `hashWithSalt` lt `hashWithSalt` tz

-- Data.Time.Locale / System.Locale

instance Hashable TimeLocale where
  hashWithSalt salt (TimeLocale a b c d e f g h) =
    salt `hashWithSalt` a
         `hashWithSalt` b
         `hashWithSalt` c
         `hashWithSalt` d
         `hashWithSalt` e
         `hashWithSalt` f
         `hashWithSalt` g
         `hashWithSalt` h
