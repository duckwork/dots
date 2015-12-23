module Colorschemes where

data Colorscheme =
     Colorscheme { fg       :: String
                 , bg       :: String
                 , black    :: String
                 , red      :: String
                 , green    :: String
                 , yellow   :: String
                 , blue     :: String
                 , magenta  :: String
                 , cyan     :: String
                 , white    :: String
                 , black'   :: String
                 , red'     :: String
                 , green'   :: String
                 , yellow'  :: String
                 , blue'    :: String
                 , magenta' :: String
                 , cyan'    :: String
                 , white'   :: String
                 }

defaultColorscheme :: Colorscheme
defaultColorscheme = 
  Colorscheme { 
    fg        = "#ffffff"
  , bg        = "#000000"
  , black     = "#000000"
  , red       = "#800000"
  , green     = "#008000"
  , yellow    = "#808000"
  , blue      = "#000080"
  , magenta   = "#800080"
  , cyan      = "#008080"
  , white     = "#c0c0c0"
  , black'    = "#000000"
  , red'      = "#ff0000"
  , green'    = "#00ff00"
  , yellow'   = "#ffff00"
  , blue'     = "#0000ff"
  , magenta'  = "#ff00ff"
  , cyan'     = "#00ffff"
  , white'    = "#ffffff"
  }
