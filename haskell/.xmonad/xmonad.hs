import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.MouseResize
import XMonad.Actions.TopicSpace
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces

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
import XMonad.Layout.WindowArranger
import XMonad.Layout.MouseResizableTile

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Input
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.AppLauncher

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Loggers

import qualified Data.Map as M

import System.IO

import qualified XMonad.StackSet as W   -- manageHook rules

main :: IO()
main = do
  status <- spawnPipe myDzenStatus
  checkTopicConfig myTopics myTopicConfig
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
    { modMask            = mod4Mask
    , terminal           = myTerminal
    , borderWidth        = 2
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#268bd2"
    , handleEventHook    = fullscreenEventHook
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , manageHook         = manageDocks <+> myManageHook
                           <+> namedScratchpadManageHook myScratchpads
                           <+> manageHook defaultConfig
    , logHook            = myLogHook status
    , startupHook = return () >> checkKeymap defaultConfig myKeys

    }
    `additionalKeysP` myKeys

myTerminal :: String
myTerminal = "urxvt"

-- myWorkspaces :: [String]
myWorkspaces =  myTopics

-- scratchpads
myScratchpads =
  [ NS "term" spawnTerm findTerm manageTerm
  , NS "bashrun" spawnBash findBash manageBash
  , NS "note" spawnNotes findNotes manageNotes
  ]
  where
    spawnTerm  = myTerminal ++ " -name scratchpad"
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

    spawnNotes  = myTerminal ++ " -name scratchpad-notes -e tail -f ~/notes"
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
  , className =? "Nitrogen"             --> doCenterFloat
  , className =? "Xfce4-mixer"          --> doCenterFloat
  , className =? "Xfce4-notifyd"        --> doIgnore
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
      mtiled = mouseResizableTileMirrored
      -- tiled    = Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
      tiled = mouseResizableTile

-- tab decoration
myTabConfig = defaultTheme
  { activeColor         = "#333333"
  , inactiveColor       = "#3C3C3C"
  , activeBorderColor   = "#333333"
  , inactiveBorderColor = "#3C3C3C"
  , activeTextColor     = "#268bd2"
  , inactiveTextColor   = "#777777"
  , decoHeight          = 25
  , fontName            = "xft:DejaVu Sans:size=10"
  }

-- prompt decoration
myXPConfig = defaultXPConfig
   { bgColor         = "#333333"
   , fgColor         = "#777777"
   , fgHLight        = "#268bd2"
   , bgHLight        = "#333333"
   , borderColor     = "#333333"
   , position        = Top
   , font            = "xft:DejaVu Sans:size=10"
   , alwaysHighlight = True
   }


-- dzen
myDzenStatus = "dzen2 -w '1150' -ta 'l'" ++ myDzenStyle
myDzenStyle  = " -dock -h '20' -fg '#777777' -bg '#333333' -fn 'xft:DejaVu Sans:size=10'"

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenPP  = dzenPP
  { ppCurrent = dzenColor "#268bd2" "" . pad
  , ppHidden  = dzenColor "#dddddd" "" . pad . noScratchPad
  , ppHiddenNoWindows = dzenColor "#777777" "" . pad . noScratchPad
  , ppUrgent  = dzenColor "#ff0000" "" . pad
  , ppSep     = " "
  , ppTitle   = dzenColor "#ffffff" "" . pad
  , ppLayout  = dzenColor "#268bd2" "" .
              (\x -> case x of
                "Full"                      -> "^i(/home/chris/.xmonad/icons/monocle.xbm)"
                "MouseResizableTile"        -> "^i(/home/chris/.xmonad/icons/tile.xbm)"
                "Mirror MouseResizableTile" -> "^i(/home/chris/.xmonad/icons/mtile.xbm)"
                "SimplestFloat"             -> "^i(/home/chris/.xmonad/icons/float.xbm)"
                "Tabbed Simplest"           -> "^i(/home/chris/.xmonad/icons/tabbed.xbm)"
              )
  }
    where
      noScratchPad ws = if ws == "NSP" then "" else ws

myTopics :: [Topic]
myTopics =
   [ "web"
   , "dotfiles"
   , "term"
   , "media"
   , "doc"
   , "irc"
   ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("web", "~/")
        , ("dotfiles" , "~/dotfiles/")
        , ("term"     , "~/")
        , ("doc"      , "~/documents/")
        , ("media"    , "/media/mac-media/")
        , ("irc"      , "~/")
        ]
    -- , defaultTopicAction = const (return ())
    , defaultTopicAction = const $ runShell >*> 3
    , defaultTopic = "dashboard"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("web",       spawn "chromium")
        , ("dotfiles",  runVim)
        , ("media",     runShell)
        , ("doc",       runVim >>
                        runShell >>
                        runShell)
        , ("irc",       spawn ircCmd)
        ]
    }

ircCmd :: String
ircCmd = "xchat"

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " -cd " ++ dir

runShell :: X ()
runShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnVim :: Dir -> X ()
spawnVim dir = spawn $ myTerminal ++ " -cd " ++ dir ++ " -e vim"

runVim :: X ()
runVim = currentTopicDir myTopicConfig >>= spawnVim

spawnChrome = spawn "chromium --new-window http://xmonad.org/xmonad-docs/xmonad-contrib/"

runChrome :: X ()
runChrome = spawnChrome

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

promptedCopy :: X ()
promptedCopy = workspacePrompt myXPConfig $ windows . copy

-- keybindings
myKeys =
  [ ("M-b"             , sendMessage ToggleStruts                         )
  , ("M1-<Tab>"        , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab      )
  , ("M-<Return>"      , dwmpromote                                       )
  , ("M-S-<Return>"    , runShell                                         )
  , ("M-r"             , spawn "xmonad --restart"                         )
  , ("M-w"             , spawn "chromium"                                 )
  , ("M-S-w"           , spawn "chromium --incognito"                     )
  , ("M-o"             , namedScratchpadAction myScratchpads "term"       )
  , ("M-S-n"           , namedScratchpadAction myScratchpads "note"       )
  , ("M-i"             , runOrRaisePrompt myXPConfig                      )
  , ("M-g"             , windowPromptGoto myXPConfig                      )
  , ("M-;"             , promptedGoto                                     )
  , ("M-S-;"           , promptedShift                                    )
  , ("M-C-;"           , promptedCopy                                     )
  , ("M-S-<Backspace>" , removeWorkspace                                  )
  , ("M-'"             , toggleWS                                         )
  , ("M-a"             , currentTopicAction myTopicConfig                 )
  , ("M-["             , sendMessage ExpandSlave                          )
  , ("M-]"             , sendMessage ShrinkSlave                          )
  , ("M-S-l"           , launchApp myXPConfig "urxvt -e vim --servername" )
  ]
