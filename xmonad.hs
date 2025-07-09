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
import qualified XMonad.Util.CustomKeys        as C
import qualified XMonad.Layout.Reflect         as L
import qualified XMonad.Layout.Spacing         as L
import qualified XMonad.Layout.MultiToggle     as MT
import qualified System.Exit                   as X


main = xmonad $ def
        { borderWidth        = 1
        , layoutHook         = myLayout
        , modMask            = mod4Mask
        , keys = C.customKeys delkeys addkeys
        -- , keys               = copyWindowKeys `mappend` keys def
        , terminal           = "alacritty"
        , normalBorderColor  = "#CCCCCC"
        , startupHook         = setWMName "LG3D"
        , focusedBorderColor = "#6C71C4"
        -- add left hand Colemak-DHm home keys and keys above the home keys as
        -- workspaces. This is very useful for 48 key keyboards like the Planck
        -- which has number keys accessible via a layer (modifier).
        , workspaces =
          [ "q", "w", "f",
            "a", "r", "s"
          ]
        }
       -- `additionalKeys`
        -- myKeys

myLayout =  Full ||| ResizableTall 1 (3/100) (1/2) []



delkeys :: XConfig a -> [(KeyMask, KeySym)]
delkeys XConfig { modMask = modm } =
  [ (modm              , xK_h)
  , (modm              , xK_e)
  , (modm              , xK_j)
  , (modm              , xK_k)
  , (modm              , xK_l)
  , (modm              , xK_m)
  , (modm              , xK_n)
  -- , (modm              , xK_p)
  , (modm              , xK_q)
  , (modm              , xK_r)
  , (modm              , xK_t)
  , (modm              , xK_w)
  , (modm .|. shiftMask, xK_e)
  , (modm .|. shiftMask, xK_h)
  , (modm .|. shiftMask, xK_j)
  , (modm .|. shiftMask, xK_k)
  , (modm .|. shiftMask, xK_l)
  , (modm .|. shiftMask, xK_m)
  , (modm .|. shiftMask, xK_n)
  , (modm .|. shiftMask, xK_p)
  , (modm .|. shiftMask, xK_q)
  , (modm .|. shiftMask, xK_r)
  , (modm .|. shiftMask, xK_t)
  , (modm .|. shiftMask, xK_w)
  ]

workspaceKeys :: [KeySym]
workspaceKeys =
  [ xK_q, xK_w, xK_f,
    xK_a, xK_r, xK_s
  ]

addkeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addkeys conf@XConfig {modMask = modm} =
  -- Rebind h, j, k, l keys to Colemak-DHm keys m, n, e, i in the same
  -- positions.
  [ ((modm,               xK_n), windows W.focusDown)
  , ((modm,               xK_e), windows W.focusUp)
  , ((modm,               xK_i), sendMessage Expand)
  , ((modm,               xK_m), sendMessage Shrink)
  , ((modm .|. shiftMask, xK_n), windows W.swapDown)
  , ((modm .|. shiftMask, xK_e), windows W.swapUp)
  , ((modm .|. shiftMask, xK_m), windows W.focusMaster)

  , ((modm, xK_u), nextWS)
  , ((modm, xK_l), prevWS)
  , ((modm .|. shiftMask, xK_u),  shiftToNext)
  , ((modm .|. shiftMask, xK_l),  shiftToPrev)
  
  , ((modm, xK_g ),   withFocused toggleBorder)
  , ((modm .|. shiftMask, xK_h ), kill1)
  , ((modm, xK_o), sendMessage MirrorShrink)
  , ((modm, xK_slash), sendMessage MirrorExpand)
  
  --
  , ((modm .|. shiftMask, xK_Return), windows W.shiftMaster)
  , ((modm,               xK_Return), spawn "alacritty")
  , ((modm,               xK_y), spawn "dmenu_run")
  , ((modm,               xK_j), spawn "emacsclient -c")
  , ((modm,               xK_b), spawn "chromium")
  
  -- restart or kill
  , ((modm,                 xK_k), spawn "xmonad --recompile && xmonad --restart")
  , ((modm .|. shiftMask, xK_k), io X.exitSuccess)
  
  -- My mnemonic for 'd' is drop floating window
  , ((modm, xK_d), withFocused $ windows . W.sink)
  -- Resize viewed windows to the correct size. I've never actually needed
  -- this. I'm not sure in what situation this would be useful.
  , ((modm, xK_z), refresh)
  -- Reflect layout across the x axis. There is a REFLECTY, but it flips the top
  -- highlight bar.
  , ((modm, xK_x), sendMessage $ MT.Toggle L.REFLECTX)
  -- lock screen
  -- , ((modm, xK_l), spawn "betterlockscreen --lock dim")
  -- rofi keybindings
  -- , ((controlMask              , xK_space), spawn "rofi -show combi -combi-modi 'drn,run,ssh' -modi combi -show-icons")
  -- , ((controlMask .|. shiftMask, xK_space), spawn "rofi -show p -modi 'p:rofi-power-menu --choices=reboot/shutdown/logout/suspend'")
  -- https://github.com/svenstaro/rofi-calc
  -- , ((modm,                      xK_c),     spawn "rofi -show calc -modi calc -no-show-match -no-sort")
  ] ++
  -- This is using a list comprehension to build a list of workspace key
  -- bindings.
  [ ((modm .|. m, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys,
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]


-- myKeys = [ ((0, 0x1008ff13), spawn "pamixer --increase 5")
--     , ((0, 0x1008ff11), spawn "pamixer --decrease 5")
--     , ((0, 0x1008FF02), spawn "xbacklight -inc 20")
--     , ((0, 0x1008FF2E), spawn "xbacklight -inc 20")
--     , ((0, 0x1008FF03), spawn "xbacklight -dec 20")
--     , ((mod4Mask, xK_u), nextWS)
--     , ((mod4Mask, xK_l), prevWS)
--     , ((mod4Mask, xK_e), nextWS)
--     , ((mod4Mask, xK_n), prevWS)
--     -- , ((mod4Mask, xK_o), shiftToNext)
--     -- , ((mod4Mask, xK_y), shiftToPrev)
--     , ((mod4Mask, xK_semicolon), sendMessage MirrorShrink)
--     , ((mod4Mask, xK_slash), sendMessage MirrorExpand)
--     , ((mod4Mask .|. shiftMask, xK_k ), kill1)
--     , ((mod4Mask .|. shiftMask, xK_u),  shiftToNext)
--     , ((mod4Mask .|. shiftMask, xK_l),  shiftToPrev)
--     -- , ((mod4Mask, xK_v ), windows copyToAll)
--     -- , ((mod4Mask .|. shiftMask, xK_v ),  killAllOtherCopies)
--     , ((mod4Mask,  xK_g ),   withFocused toggleBorder)
--     ]

-- copyWindowKeys :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
-- copyWindowKeys conf =
--     M.fromList $
--          [((m .|. mod4Mask, k), windows $ f i)
--          | (i, k) <- zip (workspaces conf) [xK_1 ..]
--          , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
