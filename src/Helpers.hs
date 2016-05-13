{--
This module contains all the helper or auxlliary functions
to be used in the main API section
-}
module Helpers where

import           Control.Applicative ()
import           Data.Monoid         ()
import qualified Data.Text           as T

import qualified Courses             as C
import qualified Instructors         as I
import qualified Partners            as P

-- Helper funcion for taking course names data out of IO
auxCourseNames :: Maybe C.CourseraResponse -> [T.Text]
auxCourseNames courses = case courses of
                           Just c  -> C.name <$> C.elements c
                           Nothing -> []

-- Helper funcion for taking short course names data out of IO
auxCourseShortNames :: Maybe C.CourseraResponse -> [T.Text]
auxCourseShortNames courses = case courses of
                                 Just c  -> C.slug <$> C.elements c
                                 Nothing -> []

-- Helper funcion for taking course id's data out of IO
auxCourseIds :: Maybe C.CourseraResponse -> [T.Text]
auxCourseIds courses = case courses of
                         Just c  -> C.id <$> C.elements c
                         Nothing -> []

-- Helper funcion for taking Instructor id's data out of IO
auxInstructorIds :: Maybe I.CourseraResponse -> [T.Text]
auxInstructorIds instructors = case instructors of
                                 Just i  -> I.id <$> I.elements i
                                 Nothing -> []

-- Helper funcion for taking Instructor's Full name data out of IO
auxInstructorName :: Maybe I.CourseraResponse -> [T.Text]
auxInstructorName instructors = case instructors of
                                  Just i  -> I.fullName <$> I.elements i
                                  Nothing -> []

-- Helper funcion for taking Partner id's data out of IO
auxPartnerIds :: Maybe P.CourseraResponse -> [T.Text]
auxPartnerIds partners = case partners of
                           Just p  -> P.id <$> P.elements p
                           Nothing -> []

-- Helper funcion for taking Partner short names data out of IO
auxPartnerShortNames :: Maybe P.CourseraResponse -> [T.Text]
auxPartnerShortNames partners = case partners of
                                  Just p  -> P.shortName <$> P.elements p
                                  Nothing -> []

-- Helper funcion for taking Partner names data out of IO
auxPartnerNames :: Maybe P.CourseraResponse -> [T.Text]
auxPartnerNames partners = case partners of
                             Just p  -> P.name <$> P.elements p
                             Nothing -> []
