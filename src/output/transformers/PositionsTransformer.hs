module Output.Transformers.PositionsTransformer (transformPositions) where

import Prelude (Bool(..), String, (.), ($), (==), (||), const, otherwise, pure)
import Control.Monad ((=<<), join)
import Data.Char (isLetter)
import Data.List (groupBy, length, map, replicate, zipWith)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, filter, find, foldr, splitOn)
import Data.Tuple (uncurry)
import Datasource.Models.Flags (Flags(..))
import Datasource.Models.AlbumResponse (AlbumResponse(..))
import Datasource.Models.TrackResponse (TrackResponse(..))
import Helpers ((◁), (◀), enumerate, fork, head', iota)
import Output.Models.EyeD3Tag (EyeD3Tag(..))

isVinyl ∷ Text → Bool
isVinyl = foldr ((||) . isLetter) False

isMultiDisc ∷ Text → Bool
isMultiDisc = isJust . find (== '-')

fromPosition ∷ Bool → Text → Text
fromPosition True α = "1"
fromPosition _    α | isVinyl α      = filter isLetter α
                    | isMultiDisc α  = fromMaybe "1" $ head' $ splitOn "-" α
                    | otherwise      = "1"

fromSingleTrack ∷ Bool → TrackResponse → [Text]
fromSingleTrack abs = pure . fromPosition abs . position

discNums ∷ Bool → Bool → TrackResponse → [Text]
discNums abs False  = fromSingleTrack abs
discNums _   True   = fork fromMaybe (fromSingleTrack True) fromSubTracks
  where fromSubTracks = map (const "1") ◁ sub_tracks

splitByDiscs ∷ Flags → AlbumResponse → [[Text]]
splitByDiscs flags = groupBy (==) . join . (discNumsWithFlags flags) ◁ tracklist
  where discNumsWithFlags = fork discNums absolute expand

-- 2D matrix of [[DiscNum, TrackNum]]
transformPositions ∷ Flags → AlbumResponse → [[EyeD3Tag]]
transformPositions flags = fork transpose tracks discs . splitByDiscs flags
  where discs      = DiscNumParameter ◁ uncurry repeatDisc ◀ enumerate
        repeatDisc = replicate . length
        tracks     = TrackNumParameter ◁ (iota =<<)
        transpose  = zipWith (\α ω → [α, ω])
