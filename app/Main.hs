{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Conduit
import Geo.GPX.Conduit
import Text.XML.Stream.Render
import Text.XML.Stream.Parse

main  :: IO ()
main = do 
    -- f <- readFile "/home/peter/gpx/test.gpx"
    -- print $ take 10 $ lines f
    let f = "/home/peter/gpx/2026-02-10-1308.gpx"
    gpx <- readGPXFile f
    print gpx
    putStrLn "-------------------------"
    -- runConduitRes $ parseFile def f .| renderBytes def .| stdoutC

