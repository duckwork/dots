-- from https://mail.haskell.org/pipermail/xmonad/2010-October/010671.html

import Data.List
import Text.EditDistance
import XMonad; import XMonad.Prompt; import XMonad.Prompt.Shell

data FuzzySpawn = FuzzySpawn deriving (Read, Show)
instance XPrompt FuzzySpawn where showXPrompt _ = "Spawn: "

fuzzySpawn cfg = do
    cmds <- io getCommands
    let compl s
        | null s = []
        | otherwise = let weight c = levenshteinDistance defaultEditCosts s c
                       in map snd $ take 20 $ sort $ map (\c -> (weight c,c)) cmds
    mkXPrompt FuzzySpawn cfg (return . compl) spawn
