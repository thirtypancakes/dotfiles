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

import XMonad.Prompt
import XMonad.Prompt.AppendFile

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Loggers

import System.IO

import qualified XMonad.StackSet as W   -- manageHook rules

main :: IO()
main = do
        status <- spawnPipe myDzenStatus
        xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
            { modMask            = mod4Mask
            , terminal           = myTerminal
            , borderWidth        = 1
            , normalBorderColor  = "#dddddd"
            , focusedBorderColor = "#0000ff"
            , handleEventHook    = fullscreenEventHook
            , workspaces = myWorkspaces
            , layoutHook = myLayoutHook
            , manageHook = manageDocks <+> myManageHook
                           <+> namedScratchpadManageHook myScratchpads
                           <+> manageHook defaultConfig
            , logHook    = myLogHook status
            }
            `additionalKeysP` myKeys

myTerminal :: String
myTerminal = "urxvt"

myWorkspaces :: [String]
myWorkspaces =  ["web","term","music","doc","float","irc"]

myScratchpads = [ NS "term" spawnTerm findTerm manageTerm
                , NS "bashrun" spawnBash findBash manageBash
                ]
  where
    spawnTerm = myTerminal ++ " -name scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.2
        w = 0.5
        t = 0.05
        l = (1 - w)/2

    spawnBash = "bashrun2"
    findBash = resource =? "bashrun2-run-dialog"
    manageBash = customFloating $ W.RationalRect l t w h
      where
        h = 0.03
        w = 0.3
        t = 0.05
        l = (1 - w)/2

myLayoutHook = avoidStruts
  $ mouseResize
  $ windowArrange
  $ smartBorders
  $ onWorkspace "web" ( myTabbed ||| full ||| mtiled ||| tiled ||| float )
  $ onWorkspace "float" ( float ||| myTabbed ||| mtiled ||| tiled ||| full )
  $ ( mtiled ||| tiled ||| full ||| myTabbed ||| float )
    where
      myTabbed  = tabbed shrinkText myTabConfig
      float   = simplestFloat
      full    = Full
      mtiled  = Mirror tiled
      tiled   = Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))

myTabConfig = defaultTheme { activeColor         = "#222222"
                           , inactiveColor       = "##22222"
                           , activeBorderColor   = "#222222"
                           , inactiveBorderColor = "#222222"
                           , activeTextColor     = "#3399ff"
                           , inactiveTextColor   = "#777777"
                           , decoHeight          = 20
                           , fontName            = "xft:DejaVui Sans:size=8"
                           }

myManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Vlc"            --> doFloat
  , className =? "stalonetray"    --> doIgnore
  , className =? "Nm-connection-editor" --> doCenterFloat
  , className =? "Wrapper" --> doFloatAt 0.835 0.02
  , className =? "Nitrogen" --> doCenterFloat
  , className =? "Xchat" --> doF (W.shift (myWorkspaces !! 4))
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)
  , isDialog  --> doCenterFloat
  ]

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -w '1215' -ta 'l'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn 'xft:DejaVu Sans:size=10'"

myDzenPP  = dzenPP
  { ppCurrent = dzenColor "#3399ff" "" . pad
  , ppHidden  = dzenColor "#dddddd" "" . pad . noScratchPad
  , ppHiddenNoWindows = dzenColor "#777777" "" . pad . noScratchPad
  , ppUrgent  = dzenColor "#ff0000" "" . pad
  , ppSep     = " "
  , ppTitle   = dzenColor "#ffffff" "" . pad
  , ppLayout  = dzenColor "#3399ff" "" .
              (\x -> case x of
                "Full" -> "^i(/home/chris/.xmonad/icons/monocle.xbm)"
                "Tall" -> "^i(/home/chris/.xmonad/icons/tile.xbm)"
                "Mirror Tall" -> "^i(/home/chris/.xmonad/icons/mtile.xbm)"
                "SimplestFloat" -> "^i(/home/chris/.xmonad/icons/float.xbm)"
                "Tabbed Simplest" -> "^i(/home/chris/.xmonad/icons/tabbed.xbm)"
                
              )
  }
    where
      noScratchPad ws = if ws == "NSP" then "" else ws

myKeys = [ ("M-b"        , sendMessage ToggleStruts              ) -- toggle the status bar gap
  , ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
  , ("M-<Return>" , dwmpromote                            ) -- swap the focused window and the master window
  , ("M-<Tab>"    , toggleWS                              ) -- toggle last workspace (super-tab)
  , ("M-<Right>"  , nextWS                                ) -- go to next workspace
  , ("M-<Left>"   , prevWS                                ) -- go to prev workspace
  , ("M-S-<Right>", shiftToNext                           ) -- move client to next workspace
  , ("M-S-<Left>" , shiftToPrev                           ) -- move client to prev workspace
  , ("M-n"        , spawn "wicd-client -n"                ) -- network manager
  , ("M-r"        , spawn "xmonad --restart"              ) -- restart xmonad w/o recompiling
  , ("M-w"        , spawn "chromium"                      ) -- launch browser
  , ("M-S-w"      , spawn "chromium --incognito"          ) -- launch private browser
  , ("M-e"        , spawn "nautilus"                      ) -- launch file manager
  , ("C-M1-l"     , spawn "gnome-screensaver-command --lock"              ) -- lock screen
  , ("M-o"        , namedScratchpadAction myScratchpads "term" )
  , ("M-i"        , namedScratchpadAction myScratchpads "bashrun" )
  , ("C-M1-<Delete>" , spawn "sudo shutdown -r now"       ) -- reboot
  , ("C-M1-<Insert>" , spawn "sudo shutdown -h now"       ) -- poweroff
  , ("M-s"        , spawnAppendPrompt )
  ]
    where
      spawnAppendPrompt = do
          spawn ("date +%m/%d/%y>> ; echo '\n'>>" ++ "/home/chris/notes")
          appendFilePrompt defaultXPConfig "/home/chris/notes"
