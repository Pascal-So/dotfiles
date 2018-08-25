rofi &

xmobar &

numlockx &

# feh --bg-scale /usr/share/backgrounds/forest_path.jpg &
feh --bg-scale "$(find /usr/share/backgrounds/random | sort -R | head -n1)" &

# [[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

exec xmonad
