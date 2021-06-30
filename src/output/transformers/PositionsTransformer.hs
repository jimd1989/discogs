module Output.Transformers.PositionsTransformer where

import Control.Applicative ((<|>))
import Control.Error.Util (note)
import Control.Monad (join)
import Data.Char (isLetter)
import Data.List (find, groupBy, zipWith)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Datasource.Models.AlbumResponse (AlbumResponse, tracklist)
import Datasource.Models.TrackResponse (TrackResponse, position, sub_tracks)
import Helpers ((◁), (◀), enumerate, fork, indices, safeIx)
import Output.Models.EyeD3Parameter (EyeD3Parameter(..))

isVinyl ∷ String → Bool
isVinyl = or . map isLetter

isMultiDisc ∷ String → Bool
isMultiDisc = isJust . find (== '-')

discNum ∷ String → Either String String
discNum α | isVinyl α     = pure $ filter isLetter α
          | isMultiDisc α = safeIx "split error" 1 $ splitOn "-" α
discNum α                 = pure "1"

singleTrack ∷ TrackResponse → Either String [String]
singleTrack = pure ◁ discNum . position

subTracks ∷ TrackResponse → Either String [String]
subTracks = map (const "1") ◁ note "sub_tracks error" . sub_tracks

track ∷ Bool → TrackResponse → Either String [String]
track False α = singleTrack α
track True  α = subTracks α <|> singleTrack α

splitByDiscs ∷ Bool → AlbumResponse → Either String [[String]]
splitByDiscs expand = groupBy (==) ◁ join ◁ traverse (track expand) . tracklist

-- 2D matrix of [[DiscNum, TrackNum]]
transformPositions ∷ Bool → AlbumResponse → Either String [[EyeD3Parameter]]
transformPositions expand = fork transpose discs tracks ◁ splitByDiscs expand
  where discs          = DiscNumParameter ◁ uncurry repeatDisc ◀ enumerate
        repeatDisc α ω = replicate (length ω) α
        tracks         = TrackNumParameter ◁ (indices =<<)
        transpose      = zipWith (\α ω → [α, ω])
