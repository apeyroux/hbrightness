# Tools to control brightness via xrandr

I use it for xmonad because xbacklight didn't work with my device.

## exemple with xmonad

``` haskell
  , ((noModMask, xK_F12             ), spawn "hbrightness -m eDP-1 -a Up")
  , ((noModMask, xK_F11             ), spawn "hbrightness -m eDP-1 -a Down")
```
