rofi &

xmobar &
stalonetray &
nm-applet &

dunst &

numlockx &

# Show random desktop background
feh --bg-scale "$(find /usr/share/backgrounds/random | sort -R | head -n1)" &

# [[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# Set keyboard repeat delay (ms) and speed (Hz)
xset r rate 150 40

exec xmonad
