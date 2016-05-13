module Main where

import           CourseraAPI
import qualified Data.Text          as T
import           System.Environment

dispatch :: [(String, IO [T.Text])]
dispatch = [ ("getCourseByName", getCourseByName)
           , ("getCourseByShortName", getCourseByShortName)
           , ("getCourseById", getCourseById)
           , ("getPartnersById", getPartnersById)
           , ("getPartnerUniversities", getPartnerUniversities)
           , ("getPartnerUniversitiesByShortName", getPartnerUniversitiesByShortName)
           , ("getInstructorById", getInstructorById)
           , ("getInstructorByName", getInstructorByName)
           ]


main :: IO ()
main = do
     args <- getArgs
     programName <- getProgName
     putStrLn $ "## Response from " ++ programName ++ " ##"
     case args of
       [command] -> case lookup command dispatch of
                        (Just action) -> action >>= mapM_ print
                        Nothing -> mapM_ putStrLn ("Only the Following operations are available: " : map fst dispatch)
       [] -> mapM_ putStrLn ("Invoke any of the following operations: " : map fst dispatch)
