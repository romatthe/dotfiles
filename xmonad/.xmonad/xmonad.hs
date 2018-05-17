import XMonad

main :: IO()
main =
  xmonad def {terminal = "gnome-terminal", modMask = mod4Mask, borderWidth = 5}
