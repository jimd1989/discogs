-- Rename this to something like FormatCommands or something?
module Processing where

import Control.Monad (join)
import Data.List (transpose)
import Data.Map (elems, keys, fromListWith)
import Data.Text (unpack)
import FormatTrack (Position)
import Helpers ((◇), fork, fst', run, snd', spaceOut, thd', wrap)
import Parsing (Album(..), Track)

-- Ugly, rewrite with better names
normalize ∷ Int → Int → [Int] → [Int]
normalize _ _ []    = []
normalize m l (α:ω) | α > m     = α : normalize α α ω
                    | α < l     = normalize (m + 1) α (α:ω)
                    | α > l     = normalize (m + 1) (l + 1) (α:ω)
                    | l > m     = normalize l l (α:ω)
                    | l < m     = m : (normalize m l ω)
                    | otherwise = α : (normalize m l ω)

-- At least rename `run`. Rename function too.
absolute ∷ [Position] → [Position]
absolute = fork zip (flip replicate 1 . length) run

-- Ugly, rewrite
fixTracks ∷ [Position] → [Position]
fixTracks = join . fixRest . group . fixDiscs
  where fixDiscs = fork zip (normalize 0 0 . map fst) (map (pure . snd))
        group    = fromListWith (++)
        fixRest  = fork (zipWith (\α ω → map (α,) ω)) keys (map run . elems)

vaCheck ∷ String → String
vaCheck "Various" = "Various Artists"
vaCheck α         = α

makeArg ∷ String → (a → String) → a → String
makeArg flag f val = "-" ◇ flag ◇ " " ◇ (f val)

-- Change ω to `absolute`
commands ∷ String → Bool → Album → [String]
commands genre ω (Album {year, artist, album, tracks}) = cmds
  where
    yearArg    = makeArg "Y" show year
    aArtistArg = makeArg "b" (wrap . vaCheck . unpack) artist
    albumArg   = makeArg "A" (wrap . unpack) album
    genreArg   = makeArg "G " wrap genre
    constants  = spaceOut ["eyeD3", yearArg, aArtistArg, albumArg, genreArg]
    tracks'    = map snd' tracks
    positions  = if ω then absolute tracks' else fixTracks tracks'
    discs      = map (makeArg "d" (show . fst)) positions
    nums       = map (makeArg "n" (show . snd)) positions
    artists    = map (makeArg "a" (wrap . unpack . fst')) tracks
    titles     = map (makeArg "t" (wrap . unpack . thd')) tracks
    constants' = replicate (length tracks) constants
    cmds       = map spaceOut $ transpose [constants', discs, nums, artists, titles]
