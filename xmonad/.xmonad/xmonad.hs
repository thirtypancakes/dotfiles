import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows


import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Input

-- for calc
import XMonad.Util.Run
import Data.Char (isSpace)
-- import System.Process

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Loggers

import System.IO

import qualified XMonad.StackSet as W   -- manageHook rules

main :: IO()
main = do
  status <- spawnPipe myDzenStatus
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
    { modMask            = mod4Mask
    , terminal           = myTerminal
    , borderWidth        = 2
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#d33682"
    , handleEventHook    = fullscreenEventHook
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , manageHook         = manageDocks <+> myManageHook
                         <+> namedScratchpadManageHook myScratchpads
                         <+> manageHook defaultConfig
    , logHook            = myLogHook status
    ,  startupHook = return () >> checkKeymap defaultConfig myKeys

    }
    `additionalKeysP` myKeys

myTerminal :: String
myTerminal = "st"

myWorkspaces :: [String]
myWorkspaces =  ["web","term","music","doc","float","irc"]

-- scratchpads
myScratchpads =
  [ NS "term" spawnTerm findTerm manageTerm
  , NS "bashrun" spawnBash findBash manageBash
  , NS "note" spawnNotes findNotes manageNotes
  ]
  where
    spawnTerm  = myTerminal ++ " -c scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.2
        w = 0.5
        t = 0.05
        l = (1 - w)/2

    spawnBash  = "bashrun2"
    findBash   = resource =? "bashrun2-run-dialog"
    manageBash = customFloating $ W.RationalRect l t w h
      where
        h = 0.03
        w = 0.3
        t = 0.05
        l = (1 - w)/2

    spawnNotes  = myTerminal ++ " -c scratchpad-notes -e tail -f ~/notes"
    findNotes   = resource =? "scratchpad-notes"
    manageNotes = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.3
        t = 0.05
        l = (1 - w)/2

-- managehook
myManageHook = composeAll
  [ className =? "MPlayer"              --> doFloat
  , className =? "Gnome-mplayer"        --> doCenterFloat
  , className =? "Nm-connection-editor" --> doCenterFloat
  -- , className =? "Wrapper"              --> doFloatAt 0.835 0.80
  , className =? "Nitrogen"             --> doCenterFloat
  , className =? "Xchat"                --> doF (W.shift (myWorkspaces !! 4))
  , className =? "Xfce4-mixer"          --> doCenterFloat
  , isFullscreen                        --> doFullFloat
  , isDialog                            --> doCenterFloat
  ]

-- layouts
myLayoutHook = avoidStruts
  $ mouseResize
  $ windowArrange
  $ smartBorders
  $ onWorkspace "web"   ( myTabbed ||| full ||| tiled ||| mtiled ||| float )
  $ onWorkspace "float" ( float ||| mtiled ||| tiled ||| full ||| myTabbed )
  $                     ( tiled ||| mtiled ||| full ||| myTabbed ||| float )
    where
      myTabbed = tabbed shrinkText myTabConfig
      float    = simplestFloat
      full     = Full
      mtiled   = Mirror tiled
      tiled    = Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))

-- trying out XMonad.Layouts.SubLayouts
-- myLayoutHook = windowNavigation $ subTabbed $ boringWindows $
--   (  tiled ||| mtiled )
--     where
--       -- myTabbed = tabbed shrinkText myTabConfig
--       float    = simplestFloat
--       full     = Full
--       mtiled   = Mirror tiled
--       tiled    = Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))

-- tab decoration
myTabConfig = defaultTheme
  { activeColor         = "#2d2d2d"
  , inactiveColor       = "#2d2d2d"
  , activeBorderColor   = "#2d2d2d"
  , inactiveBorderColor = "#2d2d2d"
  , activeTextColor     = "#d33682"
  , inactiveTextColor   = "#777777"
  , decoHeight          = 25
  , fontName            = "xft:DejaVu Sans:size=10"
  }

-- prompt decoration
myXPConfig = defaultXPConfig
   { bgColor         = "#2d2d2d"
   , fgColor         = "#777777"
   , fgHLight        = "#d33682"
   , bgHLight        = "#2d2d2d"
   , borderColor     = "#2d2d2d"
   , position        = Top
   , font            = "xft:DejaVu Sans:size=10"
   , alwaysHighlight = True
   }


-- dzen
myDzenStatus = "dzen2 -w '1150' -ta 'l'" ++ myDzenStyle
myDzenStyle  = " -dock -h '20' -fg '#777777' -bg '#2d2d2d' -fn 'xft:DejaVu Sans:size=10'"

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenPP  = dzenPP
  { ppCurrent = dzenColor "#d33682" "" . pad
  , ppHidden  = dzenColor "#dddddd" "" . pad . noScratchPad
  , ppHiddenNoWindows = dzenColor "#777777" "" . pad . noScratchPad
  , ppUrgent  = dzenColor "#ff0000" "" . pad
  , ppSep     = " "
  , ppTitle   = dzenColor "#ffffff" "" . pad
  , ppLayout  = dzenColor "#d33682" "" .
              (\x -> case x of
                "Full"            -> "^i(/home/chris/.xmonad/icons/monocle.xbm)"
                "Tall"            -> "^i(/home/chris/.xmonad/icons/tile.xbm)"
                "Mirror Tall"     -> "^i(/home/chris/.xmonad/icons/mtile.xbm)"
                "SimplestFloat"   -> "^i(/home/chris/.xmonad/icons/float.xbm)"
                "Tabbed Simplest" -> "^i(/home/chris/.xmonad/icons/tabbed.xbm)"
              )
  }
    where
      noScratchPad ws = if ws == "NSP" then "" else ws

-- calculator prompt (stolen from stackoverflow)
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
  inputPrompt c (trim ans) ?+ \input ->
    liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim  = f . f
      where f = reverse . dropWhile isSpace

-- keybindings
myKeys =
  [ ("M-b"           , sendMessage ToggleStruts                    )
  , ("M1-<Tab>"      , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab )
  , ("M-<Return>"    , dwmpromote                                  )
  , ("M-r"           , spawn "xmonad --restart"                    )
  , ("M-w"           , spawn "chromium"                            )
  , ("M-S-w"         , spawn "chromium --incognito"                )
  , ("M-e"           , spawn "nautilus"                            )
  , ("M-o"           , namedScratchpadAction myScratchpads "term"  )
  , ("M-S-n"         , namedScratchpadAction myScratchpads "note"  )
  , ("M-i"           , runOrRaisePrompt myXPConfig                 )
  , ("M-n"           , spawnAppendPrompt                           )
  , ("M-c"           , calcPrompt myXPConfig "qalc"                )
  -- , ("M-C-h"           , sendMessage $ pullGroup L)
  -- , ("M-C-l"           , sendMessage $ pullGroup R)
  -- , ("M-C-k"           , sendMessage $ pullGroup U)
  -- , ("M-C-j"           , sendMessage $ pullGroup D)
  -- , ("M-C-m"           , withFocused (sendMessage . MergeAll))
  -- , ("M-C-u"           , withFocused (sendMessage . UnMerge))
  -- , ("M-C-."           , onGroup W.focusUp')
  -- , ("M-C-,"           , onGroup W.focusDown')

  ]
  where
    spawnAppendPrompt = do
        spawn ("echo '\n' >> /home/chris/notes ; date '+%m/%d/%y %H:%M'>> " ++ "/home/chris/notes")
        appendFilePrompt myXPConfig "/home/chris/notes"
