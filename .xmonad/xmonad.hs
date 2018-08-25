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

myLayoutHook = spacingRaw False (Border 4 21 4 4) True (Border 4 4 4 4) True $
               layoutHook def

myManageHook = composeAll
    [ className =? "Xmessage" --> doFloat
    , manageDocks
    ]

myConfig = desktopConfig
    { terminal           = "termite"
    , modMask            = mod4Mask
    , borderWidth        = 4
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , normalBorderColor  = "#121212"
    , focusedBorderColor = "#3a6fc4"
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , logHook            = historyHook >> ewmhDesktopsLogHook
    }
    `removeKeys`
    [ (mod4Mask, xK_space) -- using this for switching language
    ]
    `additionalKeys` myKeys


-- Cartesian product of the screenshot options
-- Shift activates fullscreen
-- Control activates file output instead of clipboard
-- This isn't really shorter than just writing out the options
-- explicitly, but it's certainly more fun :)
printscreenShortcuts :: [((KeyMask, KeySym), X ())]
printscreenShortcuts = do
    let maim_opts_region = "-s --bordersize 4 --color=0.22,0.43,0.76"
        xclip_output = " | xclip -selection clipboard -t image/png"
        file_output = " ~/pictures/screenshots/$(date +'%F-%H-%M-%S').png"

    (modFullscreen, maim_opts) <- [(0, maim_opts_region), (shiftMask, "")]
    (modOutput, output) <- [(0, xclip_output), (controlMask, file_output)]

    return ((modFullscreen .|. modOutput, xK_Print), spawn $ "maim " ++ maim_opts ++ output)

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((mod4Mask, xK_Tab), sendMessage NextLayout)
         , ((mod4Mask, xK_p ), spawn "rofi -show combi")
         ]
         ++ printscreenShortcuts


main = do
    xmonad myConfig
