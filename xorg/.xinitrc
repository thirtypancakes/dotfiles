source /etc/X11/xinit/xinitrc.d/30-dbus
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pksc11,secrets,ssh)

export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

xrdb -merge ~/.Xresources &
xfce4-panel --disable-wm-check &
xfce4-volumed &
setxkbmap -option ctrl:nocaps -layout gb -variant mac &
xsetroot -cursor_name left_ptr &
xcape &

(sleep 2s && xmodmap -e "keycode 49 = numbersign asciitilde") &

(sleep 2s && compton -b --shadow-exclude-reg x25+0-0) &

nitrogen --restore &

nm-applet &
xfce4-power-manager &

# trayer --SetDockType true --edge top --align right --width 10 --height 20 --tint 0x222222 --transparent true --alpha 0 &

exec xmonad
