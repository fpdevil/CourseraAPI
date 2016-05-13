{-# LANGUAGE DeriveGeneric #-}
module Instructors where

import qualified Data.Text    as T
import           GHC.Generics (Generic)

data Instructors = Instructors { id       :: T.Text
                               , fullName :: T.Text
                               } deriving ( Show )

data CourseraResponse = CourseraResponse { elements :: [Instructors]
                                         } deriving ( Show, Generic )
