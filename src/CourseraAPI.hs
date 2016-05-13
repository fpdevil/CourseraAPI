{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module CourseraAPI where

-- This module contains the main API section for the entire system

import           Control.Applicative        (empty)
import           Data.Aeson                 (FromJSON (..), Value (..), decode,
                                             parseJSON, (.:), (.:?))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid
import qualified Data.Text                  as T
import           Network.HTTP.Conduit

import qualified Courses                    as C
import           Helpers                    as H
import qualified Instructors                as I
import qualified Partners                   as P


 -- Some reeferences from the following link(s)
 -- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
 -- http://no-fucking-idea.com/blog/2014/03/23/shortest-way-to-work-with-json-in-haskell/
 -- http://blog.raynes.me/blog/2012/11/27/easy-json-parsing-in-haskell-with-aeson/

 -- Coursera API Information main page
 -- https://tech.coursera.org/app-platform/catalog/

 -- The base API URL's are
 -- Courses: https://api.coursera.org/api/courses.v1
 -- Partners: https://api.coursera.org/api/partners.v1
 -- Instructors: https://api.coursera.org/api/instructors.v1

-- Coursera API endpoint
courseraAPIUrl :: String
courseraAPIUrl = "https://api.coursera.org/api/"

-- Coursera API version v1
apiVersion :: String
apiVersion = ".v1"

--
-- Define a Catalog data type which would house the below 3
-- Courses, Partners, Instructors
data Catalog = Courses | Partners | Instructors deriving ( Eq )

instance Show Catalog where
    show Courses     = "courses"
    show Instructors = "instructors"
    show Partners    = "partners"

-- Write instances for FromJSON & ToJSON for the AESON's JSON parsing
instance FromJSON C.Courses where
    parseJSON (Object v) = C.Courses                <$>
                           v .: "id"                <*>
                           v .: "slug"              <*>
                           v .: "courseType"        <*>
                           v .: "name"              <*>
                           v .:? "primaryLanguages" <*>
                           v .:? "description"      <*>
                           v .:? "specializations"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = empty

instance FromJSON P.Partners where
    parseJSON (Object v) = P.Partners               <$>
                           v .: "id"                <*>
                           v .: "shortName"         <*>
                           v .: "name"
    parseJSON _          = empty

instance FromJSON I.Instructors where
    parseJSON (Object v) = I.Instructors            <$>
                           v .: "id"                <*>
                           v .: "fullName"
    parseJSON _          = empty

--
-- Make the CourseraResponse also instances of FromJSON & ToJSON
instance FromJSON C.CourseraResponse
instance FromJSON P.CourseraResponse
instance FromJSON I.CourseraResponse

apiLocator :: Catalog -> String
apiLocator c = courseraAPIUrl <> show c <> apiVersion

-- Get the JSON Data from the service URL
getCatalogJsonData :: Catalog -> IO BS.ByteString
getCatalogJsonData catalog = simpleHttp $ apiLocator catalog

-- Decode the JSON Response data to match each of the following

-- decode the Courses
decodeCourses :: BS.ByteString -> Maybe C.CourseraResponse
decodeCourses json = decode json :: Maybe C.CourseraResponse

-- decode the Partners
decodePartners :: BS.ByteString -> Maybe P.CourseraResponse
decodePartners json = decode json :: Maybe P.CourseraResponse

-- decode the Instructors
decodeInstructors :: BS.ByteString -> Maybe I.CourseraResponse
decodeInstructors json = decode json :: Maybe I.CourseraResponse

-- Below are the Primary functions for getting the data
-- from the Coursera API. Each of them get a full thunk
-- of the data available at the API location

--
-- Retrieve all Course Information in the RAW form
allCourses :: IO (Maybe C.CourseraResponse)
allCourses = let json = getCatalogJsonData Courses
             in decodeCourses <$> json

--
-- Retrieve all Instructors Information in the RAW form
allInstructors :: IO (Maybe I.CourseraResponse)
allInstructors = let json = getCatalogJsonData Instructors
                 in decodeInstructors <$> json

--
-- Retrieve all Partners Information in the RAW form
allPartners :: IO (Maybe P.CourseraResponse)
allPartners = let json = getCatalogJsonData Partners
              in decodePartners <$> json

--
-- Get the Course Names of all available Courses
getCourseByName :: IO [T.Text]
getCourseByName = H.auxCourseNames <$> allCourses

--
-- Get the short names of all available courses
getCourseByShortName :: IO [T.Text]
getCourseByShortName = H.auxCourseShortNames <$> allCourses

--
-- Get Course id's for all Courses
getCourseById :: IO [T.Text]
getCourseById = H.auxCourseIds <$> allCourses

--
-- Get Partber id's for all Coursers
getPartnersById :: IO [T.Text]
getPartnersById = H.auxPartnerIds <$> allPartners

--
-- Get all Partber Universities by their Shortnames for all Courses
getPartnerUniversitiesByShortName :: IO [T.Text]
getPartnerUniversitiesByShortName = H.auxPartnerShortNames <$> allPartners

--
-- Get all Partner University names for all Courses
getPartnerUniversities :: IO [T.Text]
getPartnerUniversities = H.auxPartnerNames <$> allPartners

--
-- Get Instructor id's for all Courses
getInstructorById :: IO [T.Text]
getInstructorById = H.auxInstructorIds <$> allInstructors

--
-- Get Instructor names for all Courses
getInstructorByName :: IO [T.Text]
getInstructorByName = H.auxInstructorName <$> allInstructors
