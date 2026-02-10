{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Geo.GPX.Conduit

main  :: IO ()
main = do 
    gpx <- readGPXFile "/home/peter/gpx/2026-02-10-1308.gpx"
    print gpx
