module Processing where

normalize ∷ Int → Int → [Int] → [Int]
normalize _ _ []    = []
normalize m l (α:ω) | α > m     = α : normalize α α ω
                    | α < l     = normalize (m + 1) α (α:ω)
                    | α > l     = normalize (m + 1) (l + 1) (α:ω)
                    | l > m     = normalize l l (α:ω)
                    | l < m     = m : (normalize m l ω)
                    | otherwise = α : (normalize m l ω)
