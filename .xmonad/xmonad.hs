import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.WorkspaceDir

import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Font
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Scratchpad


import System.IO
import Data.Char (toLower)

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W

barFont = "xft:Inconsolata:pixelsize=12"
xpFont  = barFont
bgcolor = "gray10"
fgcolor = "white"
hicolor = "goldenrod"

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

manageHook' = manageDocks 
              <+> manageHook defaultConfig 
              <+> scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35)
layoutHook' = avoidStruts $ layoutHints $ workspaceDir "~" $ layoutHook defaultConfig
logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP $ mahuronPP
            { ppOutput = hPutStrLn h
            , ppSort = fmap (scratchpadFilterOutWorkspace.) getSortByIndex
            }
            
dzenCommon = " -fg " ++ fgcolor ++ " -bg " ++ bgcolor ++ " -fn '" ++ barFont ++ "'" ++ 
             " -e 'button1=togglecollapse;'"
leftStatusBarCmd = "/usr/bin/dzen2 -xs 1 -ta l " ++ dzenCommon

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
mahuronPP = defaultPP { ppCurrent = dzenColor "black"  "gray80" . pad
                      , ppVisible = dzenColor "black"  "gray40" . pad
                      , ppHidden  = dzenColor "gray40" ""  . pad
                      , ppUrgent  = dzenColor "black"  "goldenrod" . pad
                      , ppTitle   = dzenColor "goldenrod" "" . shorten 120
                      , ppLayout  = map toLower
                      , ppSep     = " | "
                      }
xpc :: XPConfig
xpc = defaultXPConfig { font = "xft: Bistream Vera Sans Mono-10"
                      , position = Bottom
                      , bgColor  = bgcolor
                      , fgColor  = fgcolor
                      , borderColor = "black"
                      , fgHLight    = "black"
                      , bgHLight    = hicolor
                      }