{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import XMonad.Prompt
import XMonad.Prompt.XMonad

import Data.Map    (fromList)
import Data.Monoid (mappend)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit


myTerminal :: String
myTerminal = "urxvt"

lockCommand :: String
lockCommand = "i3lock -c 121212 -l bbbbbb -O 0.03 -o bbbbbb --no-input-visualisation -F 70 -R 550 -e -X 220 -Y 140"

restartXMonadCommand :: String
restartXMonadCommand = "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "term" -- <fn=1>\xf120</fn>" -- term
               , "web"  -- <fn=1>\xf0ac</fn>" -- web
               , "code" -- <fn=1>\xf121</fn>" -- code
               , "mail" -- <fn=1>\xf0e0</fn>" -- mail
               , "files"
               ] ++ map show [6..9] -- other

lookupWorkspaceNr :: WorkspaceId -> Int
lookupWorkspaceNr "term" = 1
lookupWorkspaceNr "web" = 2
lookupWorkspaceNr "code" = 3
lookupWorkspaceNr "mail" = 4
lookupWorkspaceNr "files" = 5
lookupWorkspaceNr "6" = 6
lookupWorkspaceNr "7" = 7
lookupWorkspaceNr "8" = 8
lookupWorkspaceNr "9" = 9

myLayoutHook = smartBorders $
               spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True $
               myLayout

myLayout =
    avoidStruts (
        Tall 1 (3/100) (1/2)
    ) |||
    Full

myManageHook = composeAll
    [ className =? "Xmessage"                  --> doCenterFloat
    , className =? "File Operation Progress"   --> doCenterFloat

    , className =? "Chromium"                  --> doShift (myWorkspaces !! 1)
    , className =? "Google-chrome"             --> doShift (myWorkspaces !! 1)
    , className =? "Firefox"                   --> doShift (myWorkspaces !! 1)

    , className =? "emacs"                     --> doShift (myWorkspaces !! 2)
    , className =? "subl3"                     --> doShift (myWorkspaces !! 2)
    , manageDocks
    ]

myLogHook = do
    historyHook
    ewmhDesktopsLogHook

myConfig = desktopConfig
    { terminal           = myTerminal
    , modMask            = mod4Mask
    , workspaces         = myWorkspaces
    , borderWidth        = 4
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , normalBorderColor  = "#121212"
    , focusedBorderColor = "#3a6fc4"
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , logHook            = myLogHook

    , keys               = myKeys
    }
    `additionalKeysP` addKeys


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ terminal conf)
    , ((modMask,               xK_p     ), spawn "rofi -show combi")
    , ((modMask .|. shiftMask, xK_p     ), spawn "rofi -show run")
    , ((modMask,               xK_e     ), spawn "thunar")
    , ((mod1Mask,              xK_F4    ), kill)

    , ((modMask,               xK_Tab   ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_Tab   ), setLayout $ layoutHook conf) -- reset layout
    , ((mod1Mask,              xK_Tab   ), nextMatch History (return True)) -- alt tab

    , ((modMask,               xK_n     ), refresh)

    -- move focus up or down the window stack
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- lock screen
    -- uses https://github.com/cac03/i3lock
    , ((mod1Mask .|. mod4Mask, xK_l     ), spawn lockCommand)

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- resizing the master/side ratio
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- unfloat

    -- increase or decrease number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- quit, or restart
    -- , ((modMask .|. shiftMask, xK_q     ), xmonadPromptC systemPromptCmds def)
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask,               xK_q     ), spawn restartXMonadCommand)

    ]
    ++ printscreenShortcuts conf
    ++ workspaceShortcuts conf
    ++ screenShortcuts conf

-- Cartesian product of the screenshot options
-- Shift activates fullscreen
-- Control activates file output instead of clipboard
-- This isn't really shorter than just writing out the options
-- explicitly, but it's certainly more fun :)
printscreenShortcuts :: XConfig Layout -> [((KeyMask, KeySym), X ())]
printscreenShortcuts conf@(XConfig {modMask = modMask}) = do
    let maim_opts_region = "-s --bordersize 4 --color=0.22,0.43,0.76"
        xclip_output = " | xclip -selection clipboard -t image/png"
        file_output = " ~/pictures/screenshots/$(date +'%F-%H-%M-%S').png"

    (modFullscreen, maim_opts) <- [(0, maim_opts_region), (shiftMask, "")]
    (modOutput, output) <- [(0, xclip_output), (controlMask, file_output)]

    return ((modFullscreen .|. modOutput, xK_Print), spawn $ "maim " ++ maim_opts ++ output)


-- mod-{left, right}       Switch to physical/Xinerama screens 1, 2, or 3
-- mod-shift-{left, right} Move client to screen 1, 2, or 3
screenShortcuts :: XConfig Layout -> [((KeyMask, KeySym), X ())]
screenShortcuts conf@(XConfig {modMask = modMask}) = do
    (key, screen) <- zip [xK_Left, xK_Right] [0..]
    (action, mod) <- [(W.view, 0), (W.shift, shiftMask)]

    [((modMask .|. mod, key), do
        workspaceID <- screenWorkspace screen
        whenJust workspaceID (windows . action))]

-- mod-[1..9]       Switch to workspace N
-- mod-shift-[1..9] Move client to workspace N
-- works the same with both number keys and numpad
workspaceShortcuts :: XConfig Layout -> [((KeyMask, KeySym), X ())]
workspaceShortcuts conf@(XConfig {modMask = modMask}) = do
    let numpadKeys = [xK_KP_End, xK_KP_Down, xK_KP_Page_Down, xK_KP_Left, xK_KP_Begin, xK_KP_Right, xK_KP_Home, xK_KP_Up, xK_KP_Page_Up]
    numberkeys <- [[xK_1 .. xK_9], numpadKeys]
    (key, workspace) <- zip numberkeys (workspaces conf)
    (action, mod) <- [(windows . W.greedyView, 0), ((\ws -> windows (W.shift ws) >> windows (W.greedyView ws)), shiftMask)]

    [((modMask .|. mod, key), action workspace)]

addKeys = [ ("<XF86AudioLowerVolume>",  spawn "amixer -q sset Master 2%-"   )
          , ("<XF86AudioRaiseVolume>",  spawn "amixer -q sset Master 2%+"   )
          , ("<XF86AudioMute>",         spawn "amixer -q sset Master toggle")
          , ("<XF86MonBrightnessDown>", spawn "light -U 10"                 )
          , ("<XF86MonBrightnessUp>",   spawn "light -A 10"                 )
          -- , ("<XF86AudioPlay>",   spawn "play-pause-mpd.sh"       )
          ]


-- xmobar stuff

myXmobar :: LayoutClass l Window
         => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar conf = statusBar "xmobar 2>>~/.xmobar.out.stderr >> ~/.xmobar.out" myXmobarPP (\XConfig{modMask = modm} -> (modm, xK_b )) conf

myXmobarPP :: PP
myXmobarPP = def { ppCurrent = xmobarColor "#3a6fc4" "" . wrap "[" "]"
                 , ppHidden  = \name -> "<action=`xdotool key super+" ++ show (lookupWorkspaceNr name) ++ "`><fn=1>" ++ name ++ "</fn></action>"
                 , ppTitle   = const "" -- xmobarColor "#6f6"  "" . shorten 40
                 , ppLayout  = const "" -- last . words
                 , ppVisible = wrap "(" ")"
                 , ppUrgent  = xmobarColor "#f66" "#fb5"
                 }

main = do
    xmonad =<< myXmobar myConfig
