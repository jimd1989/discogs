-- FormatPosition instead ?
module FormatTrack where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***), (+++))
import Data.Bitraversable (bisequence)
import Data.Char (isLetter, isNumber, toUpper)
import Data.Function (const)
import Data.List (find, head, tail)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Data.Tuple (uncurry)
import Text.Read (readEither)
import Helpers ((◁), (◀), (◇), (≠), fork, safeIx)

-- Unsafe uses of `read` and `head` here
-- Turn this into proper Data?
type Position = (Int, Int)

vinyl ∷ String → Position
vinyl = convert . split
  where split       = (filter isLetter) &&& (filter (not . isLetter))
        convert     = (discNumFromVinyl *** readTrack)
        readTrack α = if (α == "") then 1 else (read α)

multiDisc ∷ String → Position
multiDisc = fork (,) head (!! 1) . map read . splitOn "-" . filter (/= ' ')

singleDisc ∷ String → Position
singleDisc α = (1, read α)

defaultPosition ∷ Int → Position
defaultPosition α = (1, α)

-- rename `checkMedia`
mediaCheck ∷ Int → String → Position
mediaCheck α ω | isVinyl ω      = vinyl ω
               | isMultiDisc ω  = multiDisc ω
               | isSingleDisc ω = singleDisc ω
               | otherwise      = defaultPosition α

position ∷ Int → Text → Position
position α = mediaCheck α . unpack

-- new
-- start from vinyl and work your way up to Position', not outside-in
data Position' = Position' { discNum ∷ String, trackNum ∷ String }

discNumFromLetter ∷ Char → Int
discNumFromLetter = (subtract 64) . fromEnum . toUpper

discNumFromVinyl ∷ String → Int
discNumFromVinyl ""       = 0
discNumFromVinyl [α]      = ceiling ((fromIntegral $ discNumFromLetter α) / 2)
discNumFromVinyl (α:ω:[]) = (13 * (discNumFromLetter α)) + discNumFromVinyl [ω]
discNumFromVinyl (α:ω)    = (side α) * (degree (α:ω)) + (discNumFromVinyl ω)
  where side   = (* 13) . discNumFromLetter
        degree = (26 ^) . (subtract 2) . length

checkNum ∷ String → Either String String
checkNum α = showNum $ readNum α
  where readNum ω = readEither ω :: Either String Int
        showNum   = const (α ◇ " is not a number") +++ show

makeVinylPosition ∷ String → Either String Position'
makeVinylPosition = uncurry Position' ◁ sequence . convert . split
  where split   = (filter isLetter) &&& (filter (not . isLetter))
        convert = (show . discNumFromVinyl) *** checkNum

makeMultiDiscPosition ∷ String → Either String Position'
makeMultiDiscPosition = uncurry Position' ◁ bisequence . verify . split
  where split  = splitOn "-" . filter (≠ ' ')
        ix     = safeIx "malformed track number"
        verify = (checkNum ◀ ix 0) &&& (checkNum ◀ ix 1)

makeSingleDiscPosition ∷ String → Either String Position'
makeSingleDiscPosition = Position' "1" ◁ checkNum

isVinyl ∷ String → Bool
isVinyl = or . map isLetter

isMultiDisc ∷ String → Bool
isMultiDisc = isJust . find (== '-')

isSingleDisc ∷ String → Bool
isSingleDisc = fork (&&) (not . (== 0) . length) (and . map isNumber)

checkMedia ∷ Int → String → Either String Position'
checkMedia α ω | isVinyl ω      = makeVinylPosition ω
               | isMultiDisc ω  = makeMultiDiscPosition ω
               | isSingleDisc ω = makeSingleDiscPosition ω
               | otherwise      = pure $ Position' "1" (show α)

makePosition ∷ Int → Text → Either String Position'
makePosition fallbackTrackNum = checkMedia fallbackTrackNum . unpack
