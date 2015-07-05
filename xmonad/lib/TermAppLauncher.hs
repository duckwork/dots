module TermAppLauncher ( launchAppInTerm
                       , module XMonad.Prompt
                       , TermApp, TAPrompt
                       ) where

import           Codec.Binary.UTF8.String (encodeString)
import           System.Posix.Process     (executeFile)
import           XMonad                   (MonadIO, X ())
import           XMonad.Core              (xfork)
import           XMonad.Prompt            (XPConfig (), XPrompt (showXPrompt),
                                           mkXPrompt)
import           XMonad.Prompt.Shell      (getShellCompl)

data TAPrompt = TAPrompt String

instance XPrompt TAPrompt where
    showXPrompt (TAPrompt n) = n ++ "> "

type Term = String
type TermArgs = [String]
type TermApp = String
type Parameters = String

tSpawn :: MonadIO m => Term -> TermArgs -> String -> m ()
tSpawn t ta x = xfork (executeFile t
                                   True
                                   (ta ++ ["-e", encodeString x])
                                   Nothing
                      ) >> return ()

tLaunch :: MonadIO m => TermApp -> Parameters -> m ()
tLaunch app params = tSpawn "termite" ["-t", app] (app ++ " " ++ params)

launchAppInTerm :: XPConfig -> TermApp -> X ()
launchAppInTerm conf app = mkXPrompt (TAPrompt app) conf (getShellCompl [])
                         $ tLaunch app
