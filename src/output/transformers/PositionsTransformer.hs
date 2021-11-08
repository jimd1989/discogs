module Output.Transformers.PositionsTransformer where

import Prelude (Bool(..), Int, String, (.), ($), (+), (*), (/), (^), (==), (||),
                ceiling, const, fromEnum, fromIntegral, otherwise, pure,
                show, subtract)
import Control.Monad ((=<<), join)
import Data.Char (Char, isLetter, toUpper)
import Data.List (groupBy, head, length, map, replicate, zipWith)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, filter, find, foldr, pack, splitOn, unpack)
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

letterDisc ∷ Char → Int
letterDisc = (subtract 64) . fromEnum . toUpper

vinylDisc ∷ String → Int
vinylDisc ""       = 0
vinylDisc [α]      = ceiling ((fromIntegral $ letterDisc α) / 2)
vinylDisc (α:ω:[]) = (13 * (letterDisc α)) + vinylDisc [ω]
vinylDisc (α:ω   ) = (side α) * (degree (α:ω)) + vinylDisc ω
  where side   = (* 13) . letterDisc
        degree = (26 ^) . (subtract 2) . length

fromVinyl ∷ Text → Text
fromVinyl = pack . show . vinylDisc . unpack . filter isLetter

fromPosition ∷ Bool → Text → Text
fromPosition True α = "1"
fromPosition _    α | isVinyl α      = fromVinyl α
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
