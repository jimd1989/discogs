module Processing where

import Control.Monad (join, liftM2)
import Data.List (intercalate, transpose)
import Data.Map (elems, keys, fromListWith)
import Data.Text (unpack)
import FormatTrack (Position)
import Helpers (fst', run, snd', thd', wrap)
import Parsing (Album(..), Track)

normalize ∷ Int → Int → [Int] → [Int]
normalize _ _ []    = []
normalize m l (α:ω) | α > m     = α : normalize α α ω
                    | α < l     = normalize (m + 1) α (α:ω)
                    | α > l     = normalize (m + 1) (l + 1) (α:ω)
                    | l > m     = normalize l l (α:ω)
                    | l < m     = m : (normalize m l ω)
                    | otherwise = α : (normalize m l ω)

absolute ∷ [Position] → [Position]
absolute = liftM2 zip (flip replicate 1 . length) run

fixTracks ∷ [Position] → [Position]
fixTracks = join . fixRest . group . fixDiscs
  where fixDiscs = liftM2 zip (normalize 0 0 . map fst) (map (pure . snd))
        group    = fromListWith (++)
        fixRest  = liftM2 (zipWith (\α ω → map (α,) ω)) keys (map run . elems)

various ∷ String → String
various α = if (α == "Various") then "Various Artists" else α

commands ∷ String → Bool → Album → [String]
commands α ω (Album {year, artist, album, tracks}) = cmds
  where
    space = intercalate " "
    year' = "-Y " <> (show year)
    albumArtist = "-b " <> (wrap $ various $ unpack artist)
    album' = "-A " <> (wrap $ unpack album)
    genre = "-G " <> (wrap α)
    constants = space ["eyeD3", year', albumArtist, album', genre]
    tracks' = map snd' tracks
    positions = if ω then absolute tracks' else fixTracks tracks'
    discs = map (("-d " <>) . show . fst) positions
    nums = map (("-n " <>) . show . snd) positions
    artists = map (("-a " <>) . wrap . unpack . fst') tracks
    titles = map (("-t " <>) . wrap . unpack . thd') tracks
    constants' = replicate (length tracks) constants
    cmds = map space $ transpose [constants', discs, nums, artists, titles]
