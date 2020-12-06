{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Hashable.Time
-- Description : Hashable instances for Data.Time types
-- License     : BSD3
-- Maintainer  : Alexey Karakulov <ankarakulov@gmail.com>
module Data.Hashable.Time (Hashable(..)) where

import Data.Hashable (Hashable(..))
import Data.Time.Compat (UniversalTime (..), DiffTime, UTCTime (..),
                         NominalDiffTime, Day (..), DayOfWeek (..), TimeZone (..),
                         TimeOfDay (..), LocalTime (..), ZonedTime (..))
import Data.Time.Calendar.Month.Compat (Month (..))
import Data.Time.Calendar.Quarter.Compat (Quarter (..), QuarterOfYear (..))

-- time-compat doesn't redefine TimeLocale
#ifdef MIN_VERSION_old_locale
import System.Locale (TimeLocale (..))
#else
import Data.Time.Format.Compat (TimeLocale (..))
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

instance Hashable Month where
  hashWithSalt salt (MkMonth x) = hashWithSalt salt x

instance Hashable Quarter where
  hashWithSalt salt (MkQuarter x) = hashWithSalt salt x

instance Hashable DayOfWeek where
  hashWithSalt salt = hashWithSalt salt . fromEnum

instance Hashable QuarterOfYear where
  hashWithSalt salt = hashWithSalt salt . fromEnum

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
