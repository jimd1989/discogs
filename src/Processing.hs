module Processing where

import Data.List (intercalate, transpose)
import Data.Text (unpack)
import FormatTrack (Position)
import Helpers (fst', snd', thd', wrap)
import Parsing (Album(..), Track)

normalize ∷ Int → Int → [Int] → [Int]
normalize _ _ []    = []
normalize m l (α:ω) | α > m     = α : normalize α α ω
                    | α < l     = normalize (m + 1) α (α:ω)
                    | α > l     = normalize (m + 1) (l + 1) (α:ω)
                    | l > m     = normalize l l (α:ω)
                    | l < m     = m : (normalize m l ω)
                    | otherwise = α : (normalize m l ω)

absolute ∷ Bool → [Position] → [Position] 
absolute False α = α
absolute True  α = zip (replicate ω 1) [1 .. ω]
  where ω = length α

various ∷ [Char] → [Char]
various α = if (α == "Various") then "Various Artists" else α

commands ∷ [Char] → Bool → Album → [[Char]]
commands α ω (Album {year, artist, album, tracks}) = cmds
  where
    space = intercalate " "
    year' = "-Y " <> (show year)
    albumArtist = "-b " <> (wrap $ various $ unpack artist)
    album' = "-A " <> (wrap $ unpack album)
    genre = "-G " <> (wrap α)
    constants = space ["eyeD3", year', albumArtist, album', genre]
    positions = absolute ω $ map snd' tracks
    discs = map (("-d " <>) . show) $ normalize 0 0 $ map fst positions
    nums = map (("-n " <>) . show . snd) positions
    artists = map (("-a " <>) . wrap . unpack . fst') tracks
    titles = map (("-t " <>) . wrap . unpack . thd') tracks
    constants' = replicate (length tracks) constants
    cmds = map space $ transpose [constants', discs, nums, artists, titles]
