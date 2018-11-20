[[ -f ~/.bashrc ]] && . ~/.bashrc

setterm -blength 0

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    # Run startx without `exec` so that when we quit xorg
    # we get dropped to the shell, not back to the login.    
    # exec startx

    startx
fi
