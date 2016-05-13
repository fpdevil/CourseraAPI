{-# LANGUAGE DeriveGeneric #-}
module Courses where

import qualified Data.Text    as T
import           GHC.Generics (Generic)

-- Data representation of the Courses Info from Coursera
-- https://api.coursera.org/api/courses.v1
data Courses = Courses { id               :: T.Text
                       , slug             :: T.Text
                       , courseType       :: T.Text
                       , name             :: T.Text
                       , primaryLanguages :: Maybe [T.Text]
                       , description      :: Maybe T.Text
                       , specializations  :: Maybe [T.Text]
                       } deriving ( Show )

-- Data representation of the complete response from Coursera
data CourseraResponse = CourseraResponse { elements :: [Courses]
                                         } deriving ( Show, Generic )
