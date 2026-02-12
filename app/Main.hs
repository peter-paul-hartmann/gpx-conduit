{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Conduit
import Geo.GPX.Conduit
import Text.XML.Stream.Render
import Text.XML.Stream.Parse
import System.Directory
import System.IO
import System.FilePath
import Data.Text (toLower, pack)
import Control.Exception (catch)
import Control.Monad (filterM)
import Data.Maybe (isJust)
import System.Environment

data STATUS = SUCCESS FilePath | FAILURE FilePath | EXCEPTION FilePath
  deriving (Show, Eq)

gpxDir :: FilePath
gpxDir = "/home/peter/gpx"

gpxFile:: FilePath -> FilePath
gpxFile f = gpxDir </> f

main  :: IO ()
main = do 
    args <- getArgs
    if (length args == 0) 
        then usage
        else
            if (head args == "m") 
                then massTest
                else do fe <- doesFileExist $ gpxFile (head args) 
                        if fe
                            then singleTest $ gpxFile $ head args
                            else putStrLn ("File " ++ (head args) ++ " does not exist.")

usage :: IO ()
usage = putStrLn "main m | main <filename of gpx file in /home/peter/gpx>"

singleTest :: FilePath -> IO ()
singleTest gf = do 
    gpx <- readGPXFile gf
    print gpx
    putStrLn ("Processing file '" ++ gf ++ "'->result=" ++ (if isJust gpx then "success" else "failure"))
    putStrLn "-------------------------"

massTest :: IO ()
massTest = 
    listDirectory gpxDir 
        >>= filterM (return . (".gpx"==) . toLower . pack . takeExtension) 
        >>= mapM (processGpx . (gpxDir </>))
        >>= outputResult

outputResult :: [ STATUS ] -> IO ()
outputResult xs = do
     putStrLn (concat [ "Total no. of processed files: "
                      , show $ length xs 
                      , "\n  No. of Successes: "
                      , show $ length $ filter isSuccess xs 
                      , "\n  No. of Failures: "
                      , show $ length $ filter isFailure xs 
                      , "\n  No. of Exceptions: "
                      , show $ length $ filter isException xs 
                      ]
              )
     putStrLn ">>>Failed files: "
     mapM_ (putStrLn . show) $ filter isFailure xs
     putStrLn ">>>files with exception: "
     mapM_ (putStrLn . show) $ filter isException xs

isSuccess :: STATUS -> Bool
isSuccess (SUCCESS _) = True
isSuccess _ = False

isFailure :: STATUS -> Bool
isFailure (FAILURE _) = True
isFailure _ = False

isException :: STATUS -> Bool
isException (EXCEPTION _) = True
isException _ = False

processGpx :: FilePath -> IO (STATUS)
processGpx f = do
    catch (do
        gpx <- readGPXFile f
        case gpx of
            Just _ -> return $ SUCCESS f
            Nothing -> return $ FAILURE f
        ) (handleXmlException f)

handleXmlException :: FilePath -> XmlException -> IO (STATUS)
handleXmlException f ex = do
    putStrLn $ "Exception in " ++ f ++ ": " ++ (show ex)
    return $ EXCEPTION f
