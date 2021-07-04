module Output.Transformers.PositionsTransformer where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Char (isLetter)
import Data.List (find, groupBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (uncurry)
import Datasource.Models.Arguments (Flags(..), absolute, expand)
import Datasource.Models.AlbumResponse (AlbumResponse, tracklist)
import Datasource.Models.TrackResponse (TrackResponse, position, sub_tracks)
import Helpers ((◁), (◀), enumerate, fork, head', iota)
import Output.Models.EyeD3Tag (EyeD3Tag(..))

isVinyl ∷ String → Bool
isVinyl = or . map isLetter

isMultiDisc ∷ String → Bool
isMultiDisc = isJust . find (== '-')

fromPosition ∷ Bool → String → String
fromPosition True α = "1"
fromPosition _    α | isVinyl α     = filter isLetter α
                    | isMultiDisc α = fromMaybe "1" $ head' $ splitOn "-" α
                    | otherwise     = "1"

fromSingleTrack ∷ Bool → TrackResponse → [String]
fromSingleTrack abs = pure . fromPosition abs . position

discNums ∷ Bool → Bool → TrackResponse → [String]
discNums abs False  = fromSingleTrack abs
discNums _   True   = fork fromMaybe (fromSingleTrack True) fromSubTracks
  where fromSubTracks = map (const "1") ◁ sub_tracks

splitByDiscs ∷ Flags → AlbumResponse → [[String]]
splitByDiscs flags = groupBy (==) . join . discNumsWithFlags ◁ tracklist
  where discNumsWithFlags = (uncurry discNums) (absolute &&& expand $ flags)

-- 2D matrix of [[DiscNum, TrackNum]]
transformPositions ∷ Flags → AlbumResponse → [[EyeD3Tag]]
transformPositions flags = fork transpose tracks discs . splitByDiscs flags
  where discs      = DiscNumParameter ◁ uncurry repeatDisc ◀ enumerate
        repeatDisc = replicate . length
        tracks     = TrackNumParameter ◁ (iota =<<)
        transpose  = zipWith (\α ω → [α, ω])
