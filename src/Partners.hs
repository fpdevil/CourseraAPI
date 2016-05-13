{-# LANGUAGE DeriveGeneric #-}
module Partners where

import qualified Data.Text    as T
import           GHC.Generics (Generic)

data Partners = Partners { id        :: T.Text
                         , shortName :: T.Text
                         , name      :: T.Text
                         } deriving ( Show )


data CourseraResponse = CourseraResponse { elements :: [Partners]
                                         } deriving ( Show, Generic )
