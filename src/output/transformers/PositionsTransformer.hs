module Output.Transformers.PositionsTransformer where

import Control.Monad (join)
import Data.Char (isLetter)
import Data.List (find, groupBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (uncurry)
import Datasource.Models.AlbumResponse (AlbumResponse, tracklist)
import Datasource.Models.TrackResponse (TrackResponse, position, sub_tracks)
import Helpers ((◁), (◀), enumerate, fork, head', iota)
import Output.Models.EyeD3Tag (EyeD3Tag(..))

isVinyl ∷ String → Bool
isVinyl = or . map isLetter

isMultiDisc ∷ String → Bool
isMultiDisc = isJust . find (== '-')

fromPosition ∷ String → String
fromPosition α | isVinyl α     = filter isLetter α
               | isMultiDisc α = fromMaybe "1" $ head' $ splitOn " " α
fromPosition α                 = "1"

fromSingleTrack ∷ TrackResponse → [String]
fromSingleTrack = pure . fromPosition . position

discNums ∷ Bool → TrackResponse → [String]
discNums False = fromSingleTrack
discNums True  = fork fromMaybe fromSingleTrack fromSubTracks
  where fromSubTracks = map (const "1") ◁ sub_tracks

splitByDiscs ∷ Bool → AlbumResponse → [[String]]
splitByDiscs expand = groupBy (==) . join . discNums expand ◁ tracklist

-- 2D matrix of [[DiscNum, TrackNum]]
transformPositions ∷ Bool → AlbumResponse → [[EyeD3Tag]]
transformPositions expand = fork transpose tracks discs . splitByDiscs expand
  where discs      = DiscNumParameter ◁ uncurry repeatDisc ◀ enumerate
        repeatDisc = replicate . length
        tracks     = TrackNumParameter ◁ (iota =<<)
        transpose  = zipWith (\α ω → [α, ω])
