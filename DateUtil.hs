------------------------------------------------------------------------------
-- | This module does date/time operations.
-- See LICENSE file for copyright and license info.
------------------------------------------------------------------------------
module DateUtil where

--||| Imports
import Date.Time.Calendar
import Date.Time.LocalTime


mkDate dd mm yyyy =
    LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight
    
mkDateTime dd mm yyyy hh nn =
    LocalTime (fromGregorian (fromIntegral yyyy) mm dd)
              (dayFractionToTimeOfDay ((hh*60+nn)/1440))
              
mkSeconds ss = LocalTime (fromGregorian (fromIntegral 2009) 11 23)
                         (dayFractionToTimeOfDay (((14*60+32)*60+ss)/(1440*60)))
