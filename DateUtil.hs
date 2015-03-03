------------------------------------------------------------------------------
-- | This module does date/time operations.
-- See LICENSE file for copyright and license info.
------------------------------------------------------------------------------
module DateUtil where

-----------------------------------------------------------------------------
-- ||| Imports
-----------------------------------------------------------------------------
import Data.Time.Calendar
import Data.Time.LocalTime

-----------------------------------------------------------------------------
-- | mkDate
-- | Create a date from dd mm yyyy data.
-----------------------------------------------------------------------------
mkDate dd mm yyyy =
    LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight
    
-----------------------------------------------------------------------------
-- | mkDateTime
-- | Create a datetime from dd mm yyyy hh nn data.
-----------------------------------------------------------------------------
mkDateTime dd mm yyyy hh nn =
    LocalTime (fromGregorian (fromIntegral yyyy) mm dd)
              (dayFractionToTimeOfDay ((hh*60+nn)/1440))
              
-----------------------------------------------------------------------------
-- | mkSeconds
-- | Create localtime from given seconds.
-----------------------------------------------------------------------------
mkSeconds ss = LocalTime (fromGregorian (fromIntegral 2009) 11 23)
                         (dayFractionToTimeOfDay (((14*60+32)*60+ss)/(1440*60)))

-----------------------------------------------------------------------------
-- | datestringToLocalTime
-- | Create localtime from "yyyy-mm-dd" string.
-----------------------------------------------------------------------------
datestringToLocalTime a_datestring = 
    readTime defaultTimeLocale "%Y-%m-%d" a_datestring :: UTCTime
