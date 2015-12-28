-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run(spawnPipe,safeSpawn)
import XMonad.Util.EZConfig(mkKeymap,checkKeymap)
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.WindowProperties
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/urxvt"

-- The command to lock the screen or show the screensaver.
myScreensaver = "/usr/bin/gnome-screensaver-command --lock"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot = "screenshot"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "$(yeganesh -x -- -fn '-*-droid sans-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#002b36' -nf '#839496' -sb '#7C7C7C' -sf '#CEFFAC')"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:term","2:web","3:code","4:vm","5:media","6:chat"] ++ map show [7..9]


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "google-chrome"  --> doShift "2:web"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "VirtualBox"     --> doShift "4:vm"
    , className =? "Xchat"          --> doShift "5:media"
    , className =? "spotify"        --> doShift "5:media"
    , className =? "slack"          --> doShift "6:chat"
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)


------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--

yellowColor     =  "#b58900"
orangeColor     =  "#cb4b16"
redColor        =  "#dc322f"
magentaColor    =  "#d33682"
violetColor     =  "#6c71c4"
blueColor       =  "#268bd2"
cyanColor       =  "#2aa198"
greenColor      =  "#859900"
baseColor03     =  "#002b36"
baseColor02     =  "#073642"
baseColor01     =  "#586e75"
baseColor00     =  "#657b83"
baseColor0      =  "#839496"
baseColor1      =  "#93a1a1"
baseColor2      =  "#eee8d5"
baseColor3      =  "#fdf6e3"

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = magentaColor

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 2


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask


myKeys = \c -> mkKeymap c $
  [ ("M-S-<Return>", spawn $ XMonad.terminal c)
  , ("M-C-l", spawn myScreensaver)
  , ("M-<Space>", spawn myLauncher)
  , ("M-S-s", spawn mySelectScreenshot)
  , ("M-S-C-s", spawn myScreenshot)

  -- volume.
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 10%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 10%-")
  , ("<XF86AudioMute>", spawn "amixer -q set Master 10%-")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close window
  , ("M-S-c", kill)

  -- Cycle layouts
  , ("M-S-<Space>", sendMessage NextLayout)
  , ("M-<Backspace>", setLayout $ XMonad.layoutHook c)

  -- Resize viewed windows to the correct size.
  , ("M-r", refresh)

  -- Move focus to the next window.
  , ("M-<Tab>", windows W.focusDown)
  , ("M-t", windows W.focusDown)

  -- Move focus to the prev window.
  , ("M-S-<Tab>", windows W.focusUp)
  , ("M-h", windows W.focusUp)

  -- Move focus to the master window.
  , ("M-m", windows W.focusMaster)
  -- Swap the focused window and the master window.
  , ("M-<Return>", windows W.swapMaster)
  -- Swap the focused window with the next window.
  , ("M-S-t", windows W.swapDown)
  -- Swap the focused window with the previous window.
  , ("M-S-h", windows W.swapUp)

  -- Increase master area
  , ("M-d", sendMessage Expand)
  , ("M-n", sendMessage Shrink)

  -- Push window back into tiling
  , ("M-j", withFocused $ windows . W.sink)

  -- Modify the number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))

  -- Exit xmonad
  , ("M-S-q", io (exitWith ExitSuccess))
  -- Restart xmonad
  , ("M-q", spawn myRestart)
  
  -- Copy/Paste shortcuts
  , ("M-c", windowBasedCopy)
  , ("M-v", windowBasedPaste)

  , ("M-x w", spawn "xmessage 'woohoo!'")  -- type mod+x then w to pop up 'woohoo!'
  ]
  ++
  [ (otherModMasks ++ "M-" ++ key, action tag)
  | (tag, key)  <- zip myWorkspaces (map (\x -> show x) [1..9])
  , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
  , ("S-", windows . W.shift)]
                                                           ]
--   ++
--
--   -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
--   -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--   [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

windowBasedCopy = withFocused (\w -> do
  isUrxvt <- isUrxvtWindow w
  if isUrxvt
  then sendKey (mod1Mask .|. controlMask) xK_c
  else sendKey controlMask xK_c)

windowBasedPaste = withFocused (\w -> do
  isUrxvt <- isUrxvtWindow w
  if isUrxvt
  then sendKey (mod1Mask .|. controlMask) xK_v
  else sendKey controlMask xK_v)

isUrxvtWindow :: Window -> X Bool
isUrxvtWindow = hasProperty (Or (ClassName "urxvt") (ClassName "URxvt"))


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


-----------------------------------------------------------------------
-- libnotify
-- set up notifications

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read,Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap(W.findTag w) $ gets windowset

    safeSpawn "notify-send" [show name, "workspace " ++ idx]


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()-- >> checkKeymap defaults myKeys


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--

main = do
  d <- spawnPipe "dzen2 -x '0' -y '0' -w '1920' -ta l -fn 'xft:DejaVu Sans' -p"
  spawn $ "conky -c ~/.xmonad/conky-dzen | " ++
          "dzen2 -x '1920' -w '1740' -ta r -fn 'xft:DejaVu Sans' -p"
  xmonad
    $ withUrgencyHook LibNotifyUrgencyHook
    $ defaults {
      logHook = dynamicLogWithPP $ dzenPP { ppOutput   = hPutStrLn d
                                          , ppCurrent  = dzenColor baseColor3 baseColor03 . pad
                                          , ppVisible  = dzenColor baseColor02 baseColor03 . pad
                                          , ppHidden   = dzenColor baseColor0 baseColor03 . pad
                                          , ppHiddenNoWindows = dzenColor baseColor02 baseColor03 . pad
                                          , ppUrgent   = dzenColor redColor baseColor03 . pad
                                          , ppTitle    = dzenColor cyanColor baseColor03 . pad
                                          , ppLayout   = dzenColor baseColor01 baseColor03 . pad
                                          }
        -- ppHiddenNoWindows :: WorkspaceId -> String
        -- -- how to print tags of empty hidden workspaces
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
  }

-- yellowColor     =  "#b58900"
-- orangeColor     =  "#cb4b16"
-- redColor        =  "#dc322f"
-- magentaColor    =  "#d33682"
-- violetColor     =  "#6c71c4"
-- blueColor       =  "#268bd2"
-- cyanColor       =  "#2aa198"
-- greenColor      =  "#859900"
-- baseColor03     =  "#002b36"
-- baseColor02     =  "#073642"
-- baseColor01     =  "#586e75"
-- baseColor00     =  "#657b83"
-- baseColor0      =  "#839496"
-- baseColor1      =  "#93a1a1"
-- baseColor2      =  "#eee8d5"
-- baseColor3      =  "#fdf6e3"
------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}

myRestart :: String
myRestart = "for pid in `pgrep dzen2`; do kill -9 $pid; done && for pid in `pgrep conky`; do kill -9 $pid; done && xmonad --recompile && xmonad --restart"
