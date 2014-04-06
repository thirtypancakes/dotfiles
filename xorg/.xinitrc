xrdb -merge ~/.Xresources &
xfce4-panel --disable-wm-check &
setxkbmap -option ctrl:nocaps -layout gb -variant mac &
xsetroot -cursor_name left_ptr &

(sleep 2s && xmodmap -e "keycode 49 = numbersign asciitilde") &

compton -b &

nitrogen --restore &

nm-applet &
xfce4-power-manager &
volti &

# trayer --SetDockType true --edge top --align right --width 10 --height 20 --tint 0x222222 --transparent true --alpha 0 &

exec xmonad
