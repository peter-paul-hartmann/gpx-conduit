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
import Data.Maybe (isJust)

gpxDir :: FilePath
gpxDir = "/home/peter/gpx"

main  :: IO ()
main = do 
    -- f <- readFile "/home/peter/gpx/test.gpx"
    -- print $ take 10 $ lines f
    let f = gpxDir </> "2016-08-15 07_49_14.gpx"
    gpx <- readGPXFile f
    print (if isJust gpx then "success" else "failure")
    putStrLn "-------------------------"
    -- runConduitRes $ parseFile def f .| renderBytes def .| stdoutC

massTest :: IO ()
massTest = do 
    files <- listDirectory gpxDir
    mapM_ (processGpx . (gpxDir </>)) $ filter ((".gpx"==) . toLower . pack . takeExtension) files 

processGpx :: FilePath -> IO ()
processGpx f = do
    catch (do
        gpx <- readGPXFile f
        case gpx of
            Just _ -> return ()
            Nothing -> putStrLn ("GPX " ++ f ++ " failed to process.")
        ) (handleXmlException f)

handleXmlException :: FilePath -> XmlException -> IO ()
handleXmlException f ex = putStrLn $ "Exception in " ++ f ++ ": " ++ (show ex)
