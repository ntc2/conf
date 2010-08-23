-- Gnome config from
-- http://haskell.org/haskellwiki/Xmonad/Basic_Desktop_Environment_Integration
-- see also
-- http://donsbot.wordpress.com/2010/03/13/after-3-years-my-xmonad-configuration-now-uses-gnome/
--
-- need to set up gnome to use xmonad though
-- http://haskell.org/haskellwiki/Xmonad/Using_xmonad_in_Gnome#Starting_GNOME_with_xmonad
-- change the windowmanager used by gnome (and then logout, or start a
-- new gnome with {startx -- :1} in a tty):
-- 
--   gconftool-2 -s /desktop/gnome/session/required_components/windowmanager xmonad --type string
--   gconftool-2 -s /desktop/gnome/session/required_components/windowmanager metacity --type string
--
-- this gives multihead for free.
--
-- TODO
--
-- -  tabbing or stacking
--
--    see http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-SubLayouts.html
--
-- -  disable "smart" workspace switching, whereby xmonad swaps
--    current workspace and target when both appear on monitors
--    currently
--
--    see
--    http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Multi_head_and_workspaces_.28desktops.29
--
-- -  dzen or xmobar
-- 
-- DONE
--
-- -  {aptitude install dmenu}
import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.UpdatePointer -- mouse follows focus
--import XMonad.Layout.TabBarDecoration -- tabs: sucks by default: the tabs don't do anything :P
import XMonad.Hooks.ManageHelpers -- fullscreen flash
import XMonad.Config.Desktop (desktopLayoutModifiers) -- custom layoutHook + gnome
import XMonad.Layout.NoBorders (smartBorders)

main = xmonad gnomeConfig
       { logHook = logHook gnomeConfig
                   -- mouse follows focus to middle-top
                   >> updatePointer (Relative 0.5 0.0) 
         -- i have two mod keys: the left
         -- windows key (Super_L) and the right menu key (Menu)
         -- 
         -- - use xmodmap to get list of modifiers, e.g. Super_L is a
         --   mod4 modifier.
         --
         -- - use xev to find keysym for a given key.
         --
         -- - use xmodmap to add keys to a modifier, and remap keys,
         --   e.g. adding Menu to mod4:
         --
         --   : xmodmap -e "add Mod4 = Menu"
         --   : xmodmap -e "keysym Menu = Super_L"
         --
         --   the second line alone may be enough, but I didn't do
         --   things in that order ... YES!
       , modMask     = mod4Mask
       , startupHook = spawn "xmodmap -e \"keysym Menu = Super_L\""
       , manageHook  = myManageHook -- full-screen flash
                       <+> manageHook gnomeConfig
                       -- no borders for single window
       , layoutHook  = smartBorders 
         -- http://haskell.cs.yale.edu/haskellwiki/Xmonad/Using_xmonad_in_Gnome#Layouts
                       $ desktopLayoutModifiers 
                       $ Full ||| tall ||| Mirror tall
--       , layoutHook = simpleTabBar $ layoutHook gnomeConfig
       }

-- two master, 1/10th resize increment, only show master by default
tall = Tall 2 (1/10) 1

-- fullscreen
--
-- - flash in youtube: this managehook fixes it
-- - mplayer: add {-fstype none -fs} to your command line
myManageHook = composeOne [ isFullscreen -?> doFullFloat ] 

-- http://hackage.haskell.org/packages/archive/xmonad-contrib/latest/doc/html/XMonad-Layout-Tabbed.html
-- http://haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Multi_head_and_workspaces_.28desktops.29
-- 

-- complicated config, could be educational:
--
-- http://snipt.net/doitian/xmonad-configuration/
