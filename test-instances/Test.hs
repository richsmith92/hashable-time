module Main where

import Data.Hashable (Hashable)
import Data.Hashable.Time ()
import Data.Time.Compat
import Data.Time.Calendar.Month.Compat
import Data.Time.Calendar.Quarter.Compat

main :: IO ()
main = putStrLn "OK"
 
_HashableInstances :: [()]
_HashableInstances =
    [ test (undefined :: TimeLocale)
    , test (undefined :: LocalTime)
    , test (undefined :: TimeOfDay)
    , test (undefined :: TimeZone)
    , test (undefined :: UniversalTime)
    , test (undefined :: UTCTime)
    , test (undefined :: NominalDiffTime)
    , test (undefined :: DiffTime)
    , test (undefined :: DayOfWeek)
    , test (undefined :: Day)
    , test (undefined :: QuarterOfYear)
    , test (undefined :: Quarter)
    , test (undefined :: Month)
    ]
  where
    test :: Hashable t => t -> ()
    test _ = ()
