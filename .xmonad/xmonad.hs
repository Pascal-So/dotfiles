import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import Data.Map    (fromList)
import Data.Monoid (mappend)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

myTerminal = "termite"

myWorkspaces = ["1:term","2:web","3:code"] ++ map show [4..9]

myLayoutHook = spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True $
               myLayout

myLayout = 
    avoidStruts (
        Tall 1 (3/100) (1/2)
    ) ||| 
    Full

myManageHook = composeAll
    [ className =? "Xmessage"      --> doFloat
    , className =? "Chromium"      --> doShift "2:web"
    , className =? "Google-chrome" --> doShift "2:web"
    , className =? "Firefox"       --> doShift "2:web"
    , manageDocks
    ]

myLogHook xmproc = do
    historyHook
    ewmhDesktopsLogHook

myConfig xmproc = desktopConfig
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
    , logHook            = myLogHook xmproc

    , keys               = myKeys
    }


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ terminal conf)
    , ((modMask,               xK_p     ), spawn "rofi -show combi")
    , ((modMask .|. shiftMask, xK_p     ), spawn "rofi -show run")
    , ((modMask .|. shiftMask, xK_c     ), kill)

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
    , ((mod1Mask .|. mod4Mask, xK_l     ), spawn "i3lock -c 121212 -l bbbbbb -O 0.03 -o bbbbbb --no-input-visualisation -F 45 -R 160 -e")

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- unfloat

    -- increase or decrease number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask,               xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
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
    (action, mod) <- [(W.greedyView, 0), (W.shift, shiftMask)]

    [((modMask .|. mod, key), windows $ action workspace)]


main = do
    -- xmproc <- spawnPipe "xmobar -d"
    xmonad =<< xmobar (myConfig ())
