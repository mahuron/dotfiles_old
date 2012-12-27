import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.IM

import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Font
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Scratchpad

import System.IO
import Data.Char (toLower)
import Data.Ratio ((%))

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W

solarizedBase03  = "#002b36"
solarizedBase02  = "#073642"
solarizedBase01  = "#586e75"
solarizedBase00  = "#657b83"
solarizedBase0   = "#839496"
solarizedBase1   = "#93a1a1"
solarizedBase2   = "#eee8d5"
solarizedBase3   = "#fdf6e3"
solarizedYellow  = "#b58900"
solarizedOrange  = "#cb4b16"
solarizedRed     = "#dc322f"
solarizedMagenta = "#d33682"
solarizedViolet  = "#6c71c4"
solarizedBlue    = "#268bd2"
solarizedCyan    = "#2aa198"
solarizedGreen   = "#859900"

barFont = "xft:Inconsolata:pixelsize=14"
xpFont  = barFont

bgcolor = solarizedBase03
fgcolor = solarizedBase2
hicolor = solarizedYellow

normalBorderColor' = bgcolor
focusedBorderColor' = hicolor

workspaces' = ["1:main", "2:web", "3:chat", "4:dev"] ++ map show [5 .. 9 :: Int]

terminal' = "x-terminal-emulator"
browser' = "x-www-browser"

focusFollowsMouse' = True
borderWidth' = 2

modMask' = mod4Mask
addKeys = [ ("M-s " ++ k, S.promptSearchBrowser xpc browser' f) | (k,f) <- searchList ] ++
          [ ("M-S-s " ++ k, S.selectSearchBrowser browser' f) | (k,f) <- searchList ] ++
          [ ("M-z", spawn "xscreensaver-command -lock")
          , ("M-r", runOrRaisePrompt xpc )
          , ("M-a", scratchpadSpawnAction defaultConfig { terminal = terminal' } )
          , ("M-d", changeDir xpc)
          ]

searchList :: [(String, S.SearchEngine)]
searchList = [ ("s", S.google)
             , ("g", S.searchEngine "go redir" "http://go.pdsea.f5net.com:8080/")
             , ("w", S.wikipedia)
             ]

manageHook' = (composeAll . concat $
              [ [className    =? c    --> doShift (workspaces' !! 2) | c <- chatClass ]
              , [className    =? c    --> doShift (workspaces' !! 1) | c <- webClass ]
              ])
              <+> manageDocks 
              <+> manageHook defaultConfig 
              <+> scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35)
              where
                webClass    = ["Chrome", "Firefox"]
                chatClass   = ["Pidgin", "Xchat"]

imLayout = withIM (1%6) pidginRoster chatLayout
  where
    chatLayout = Grid
    ratio = (1%6)
    rosters = [pidginRoster]
    pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
  
layoutHook' = avoidStruts $ layoutHints $ workspaceDir "~" $ im $ layoutHook defaultConfig
  where
    im = onWorkspace "3:chat" imLayout

logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP $ mahuronPP
            { ppOutput = hPutStrLn h
            , ppSort = fmap (scratchpadFilterOutWorkspace.) getSortByIndex
            }
            
leftStatusBarCmd = "/usr/bin/dzen2 -xs 1 -ta l -fn '" ++ barFont ++ "'"

main = do
  statusBar <- spawnPipe leftStatusBarCmd
  xmonad $ withUrgencyHook NoUrgencyHook
         $ defaultConfig
           { terminal           = terminal'
           , focusFollowsMouse  = focusFollowsMouse'
           , borderWidth        = borderWidth'
           , modMask            = modMask'
           , workspaces         = workspaces'
           , normalBorderColor  = normalBorderColor'
           , focusedBorderColor = focusedBorderColor'
           , manageHook         = manageHook'
           , layoutHook         = layoutHook'
           , logHook            = (logHook' statusBar)
           } `additionalKeysP` addKeys

mahuronPP :: PP
mahuronPP = defaultPP { ppCurrent = dzenColor solarizedBase01 solarizedBase3 . pad
                      , ppVisible = dzenColor solarizedBase02 solarizedBase0 . pad
                      , ppHidden  = dzenColor solarizedBase00 ""  . pad
                      , ppUrgent  = dzenColor solarizedBase02  solarizedYellow . pad
                      , ppTitle   = dzenColor solarizedYellow "" . shorten 120
                      , ppLayout  = map toLower
                      , ppSep     = " | "
                      }
xpc :: XPConfig
xpc = defaultXPConfig { font = "xft: Bistream Vera Sans Mono-10"
                      , position = Bottom
                      , bgColor  = bgcolor
                      , fgColor  = fgcolor
                      , borderColor = bgcolor
                      , fgHLight    = solarizedBase02
                      , bgHLight    = hicolor
                      }