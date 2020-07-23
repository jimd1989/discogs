module FormatTrack where

import Control.Arrow ((&&&), (***))
import Control.Monad (liftM2)
import Data.Char (isLetter, isNumber, toUpper)
import Data.List (find, head, tail)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)

type Position = (Int, Int)

letterDisc ∷ Char → Int
letterDisc = (subtract 64) . fromEnum . toUpper

vinylDisc ∷ [Char] → Int
vinylDisc ""       = 0
vinylDisc [α]      = ceiling ((fromIntegral $ letterDisc α) / 2)
vinylDisc (α:ω:[]) = (13 * (letterDisc α)) + vinylDisc [ω]
vinylDisc α        = (side α) * (degree α) + vinylDisc (tail α)
  where side   = (* 13) . letterDisc . head
        degree = (26 ^) . (subtract 2) . length

vinyl ∷ [Char] → Position
vinyl = convert . split
  where split       = (filter isLetter) &&& (filter (not . isLetter))
        convert     = (vinylDisc *** readTrack)
        readTrack α = if (α == "") then 1 else (read α)

isVinyl ∷ [Char] → Bool
isVinyl = or . map isLetter

multiDisc ∷ [Char] → Position
multiDisc = liftM2 (,) head (!! 1) . map read . splitOn "-" . filter (/= ' ')

isMultiDisc ∷ [Char] → Bool
isMultiDisc = isJust . find (== '-')

isSingleDisc ∷ [Char] → Bool
isSingleDisc = liftM2 (&&) (not . (== 0) . length) (and . map isNumber)

singleDisc ∷ [Char] → Position
singleDisc α = (1, read α)

defaultPosition ∷ Int → Position
defaultPosition α = (1, α)

mediaCheck ∷ Int → [Char] → Position
mediaCheck α ω | isVinyl ω      = vinyl ω
               | isMultiDisc ω  = multiDisc ω
               | isSingleDisc ω = singleDisc ω
               | otherwise      = defaultPosition α

position ∷ Int → Text → Position
position α = mediaCheck α . unpack
