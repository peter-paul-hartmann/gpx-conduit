{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
{-| This is a partial parsing of the GPX 1.0 and 1.0 exchange types.
 -}
module Geo.GPX.Conduit
{-
        ( Track(..), GPX(..), Segment(..), Point(..)
        , readGPXFile
        , pt
        ) -} where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad
import Conduit
import Data.Conduit.List as L
import Data.Void (Void)
import Data.Time.Format
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
import Data.String
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, parseTimeM)
import Data.XML.Types
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.Attoparsec.Text as AT

import Debug.Trace


-- |A GPX file usually is a single track (but can be many)
-- with one or more segments and many points in each segment.
data GPX = GPX  
     { -- waypoints  :: [Waypoint]
       -- , routes     :: [Route]
        metadata :: Metadata
     ,  tracks  :: [Track] 
     } deriving (Eq, Ord, Show, Read)

data Metadata = Metadata 
     { mdName   :: Text
     , mdDesc   :: Text
     } deriving (Eq, Ord, Show, Read)
     
data Track = Track 
     { trkName               :: Maybe Text
     , trkDescription        :: Maybe Text
     , segments              :: [Segment]
     } deriving (Eq, Ord, Show, Read)

-- |A GPX segments is just a bundle of points.
data Segment = Segment 
     { points  :: [Point] 
     } deriving (Eq, Ord, Show, Read)

type Latitude = Double
type Longitude = Double

-- |Track point is a full-fledged representation of all the data
-- available in most GPS loggers.  It is possible you don't want
-- all this data and can just made do with coordinates (via 'Pnt')
-- or a custom derivative.
data Point           = Point
     { pntLat        :: Latitude
     , pntLon        :: Longitude
     , pntEle        :: Maybe Double -- ^ In meters
     , pntTime       :: Maybe UTCTime
     -- , pntSpeed   :: Maybe Double -- ^ Non-standard.  Usually in meters/second.
     , pntExts       :: Maybe Extension
     } deriving (Eq, Ord, Show, Read)

data Extension       = Extension 
     { extName       :: Text
     , extValue      :: Text
     } deriving (Eq, Ord, Show, Read)

{- | code changes are inspired by the tutorial 
https://martin.hoppenheit.info/blog/2023/xml-stream-processing-with-haskell/
-}
pt :: Latitude -> Longitude -> Maybe Double -> Maybe UTCTime -> Maybe Extension -> Point
pt t g e m ext = Point t g e m ext

readGPXFile :: FilePath -> IO (Maybe GPX)
readGPXFile fp = runConduitRes $ parseFile def (fromString fp) .| readGPX'

readGPX' :: MonadThrow m => ConduitT Event Void m (Maybe GPX)
readGPX' = parseGPX

readGPX :: MonadThrow m => ConduitT Event Void m (GPX)
readGPX = force "failed reading GPX file " $ parseGPX

{- has worked except the xmlns attribute in the root element -}
parseGPX :: MonadThrow m => ConduitT Event o m (Maybe GPX)
parseGPX = 
    tagIgnoreAttrs (defNs "gpx") (do
        metadata    <- parseMetadata
        _           <- many_ $ ignoreTree (defNs "wpt") ignoreAttrs
        ts          <- many parseTrack
        return $ GPX (fromMaybe (Metadata "" "") metadata) ts)

parseMetadata :: MonadThrow m => ConduitT Event o m (Maybe Metadata)
parseMetadata = do
    tagNoAttr (defNs "metadata")  (do
        n           <- tagNoAttr (defNs "name") content
        d           <- tagNoAttr (defNs "desc") content
        _           <- ignoreTree (defNs "author") ignoreAttrs
        _           <- ignoreTree (defNs "copyright") ignoreAttrs
        _           <- ignoreTree (defNs "time") ignoreAttrs
        return (Metadata (fromMaybe "egal" n) (fromMaybe "xegal" d)))

parseTrack :: MonadThrow m => ConduitT Event o m (Maybe Track)
parseTrack = do
    tagNoAttr (defNs "trk") (do
        n           <- tagNoAttr (defNs "name") content
        -- d           <- tagNoAttr "desc" content
        _           <- ignoreTree (nsExtensions "extensions") ignoreAttrs
        segs        <- many parseSegment
        return (Track n Nothing segs))

parseSegment :: MonadThrow m => ConduitT Event o m (Maybe Segment)
parseSegment = do 
    tagNoAttr (defNs "trkseg") (do
        pnts        <- many parsePoint
        return (Segment pnts))

parsePoint :: MonadThrow m => ConduitT Event o m (Maybe Point)
parsePoint =
    tag' (defNs "trkpt") parseAttributes $ \(lat, lon) -> (do
--        ele       <- tagNoAttr "ele" content
        time        <- tagNoAttr (defNs "time") content
        exts        <- parseExtension
        return $ Point { pntLon = parseDouble lon
                       , pntLat = parseDouble lat
                       , pntTime = parseUTC (fromMaybe "2026-02-10T13:09:01.237" time)
                       , pntEle = Nothing --parseDouble <$> ele
                       , pntExts = exts 
                       })
  where
    parseAttributes = (,) <$> requireAttr "lat" <*> requireAttr "lon" <* ignoreAttrs

parseExtension :: MonadThrow m => ConduitT Event o m (Maybe Extension)
parseExtension =
    tagNoAttr (defNs "extensions") (do 
        hr          <- fromMaybe "" <$> tagNoAttr (nsExtensions "hr") content
        ca          <- fromMaybe "" <$> tagNoAttr (nsExtensions "cadence") content
        return $ Extension hr ca)

parseDouble :: Text -> Double
parseDouble l = either (const 0) id (AT.parseOnly AT.double l)

parseUTC :: Text -> Maybe UTCTime
parseUTC = either (const Nothing) id . AT.parseOnly (do 
        yearMonthDay <- AT.manyTill AT.anyChar (AT.char 'T')
        hourMinSec   <- AT.manyTill AT.anyChar (AT.choice [AT.char '.', AT.char 'Z'])
        fraction     <- AT.choice [AT.manyTill AT.anyChar (AT.char 'Z'), return ""]
        -- The Time package version 1.4 does not handle F T and Q property for
        -- buildTime.
        -- return (buildTime defaultTimeLocale 
        --   [('F', yearMonthDay), ('T', hourMinSec), ('Q', fraction)]))
        return (parseTimeM True defaultTimeLocale "%F %T %Q"
                        (unwords [yearMonthDay,hourMinSec,'.':fraction]))
        )

defNs :: Text -> NameMatcher Name
defNs n = matching (== Name n (Just "http://www.topografix.com/GPX/1/1") (Nothing))

nsExtensions :: Text -> NameMatcher Name
nsExtensions n = matching (== Name n (Just "http://www.cluetrust.com/XML/GPXDATA/1/0") (Just "gpxdata"))

