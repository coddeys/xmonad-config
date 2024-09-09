import           System.IO
import           XMonad
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Util.EZConfig    (additionalKeys)
import           XMonad.Actions.CopyWindow
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import           XMonad.Actions.NoBorders (toggleBorder)

main = xmonad $ def
        { borderWidth        = 1
        , layoutHook         = myLayout
        , modMask            = mod4Mask
        , keys               = copyWindowKeys `mappend` keys def
        , terminal           = "termonad"
        , normalBorderColor  = "#CCCCCC"
        , startupHook         = setWMName "LG3D"
        , focusedBorderColor = "#6C71C4" }
       `additionalKeys`
        myKeys

myLayout =  Full ||| ResizableTall 1 (3/100) (1/2) []


myKeys = [ ((0, 0x1008ff13), spawn "pamixer --increase 10")
    , ((0, 0x1008ff11), spawn "pamixer --decrease 10")
    , ((0, 0x1008FF02), spawn "xbacklight -inc 20")
    , ((0, 0x1008FF2E), spawn "xbacklight -inc 20")
    , ((0, 0x1008FF03), spawn "xbacklight -dec 20")
    , ((mod4Mask, xK_i), nextWS)
    , ((mod4Mask, xK_u), prevWS)
    , ((mod4Mask, xK_o), shiftToNext)
    , ((mod4Mask, xK_y), shiftToPrev)
    , ((mod4Mask, xK_semicolon), sendMessage MirrorShrink)
    , ((mod4Mask, xK_slash), sendMessage MirrorExpand)
    , ((mod4Mask, xK_n ), kill1)
    -- , ((mod4Mask .|. controlMask, xK_i),  shiftToNext >> nextWS)
    -- , ((mod4Mask .|. controlMask, xK_u),  shiftToPrev >> prevWS)
    -- , ((mod4Mask, xK_v ), windows copyToAll)
    -- , ((mod4Mask .|. shiftMask, xK_v ),  killAllOtherCopies)
    , ((mod4Mask,  xK_g ),   withFocused toggleBorder)
    ]

copyWindowKeys :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
copyWindowKeys conf =
    M.fromList $
         [((m .|. mod4Mask, k), windows $ f i)
         | (i, k) <- zip (workspaces conf) [xK_1 ..]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
