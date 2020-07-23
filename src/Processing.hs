module Processing where

import Data.List (intercalate, transpose)
import Data.Text (unpack)
import Parsing (Album(..), Track)

normalize ∷ Int → Int → [Int] → [Int]
normalize _ _ []    = []
normalize m l (α:ω) | α > m     = α : normalize α α ω
                    | α < l     = normalize (m + 1) α (α:ω)
                    | α > l     = normalize (m + 1) (l + 1) (α:ω)
                    | l > m     = normalize l l (α:ω)
                    | l < m     = m : (normalize m l ω)
                    | otherwise = α : (normalize m l ω)

various ∷ [Char] → [Char]
various α = if (α == "Various") then "Various Artists" else α

wrap ∷ [Char] → [Char]
wrap α = "\"" <> α <> "\""

fst' ∷ (a, b, c) → a
fst' (α, _, _) = α

snd' ∷ (a, b, c) → b
snd' (_, α, _) = α

thd' ∷ (a, b, c) → c
thd' (_, _, α) = α

commands ∷ [Char] → Album → [[Char]]
commands α (Album {year, artist, album, tracks}) = cmds
  where
    space = intercalate " "
    year' = "-Y " <> (show year)
    albumArtist' = "-b " <> (wrap $ various $ unpack artist)
    album' = "-A " <> (wrap $ unpack album)
    genre' = "-G " <> (wrap α)
    constants = space ["eyeD3", year', albumArtist', album', genre']
    disc' = mappend "-d " . show
    discs' = map disc' $ normalize 0 0 $ map (fst . snd') tracks
    track' = mappend "-n " . show
    tracks' = map (track' . snd . snd') tracks
    artists' = map (mappend "-a " . wrap . unpack . fst') tracks
    titles' = map (mappend "-t " . wrap . unpack . thd') tracks
    headings = replicate (length tracks) constants
    cmds = map space $ transpose [headings, discs', tracks', artists', titles']
