alias ga="git add"
alias gd="git diff"
alias gc="git commit"
alias go="git checkout"

alias fucking=sudo

function swap()         
{
    local TMPFILE=tmp.$$
    mv -i "$1" $TMPFILE && mv "$2" "$1" && mv $TMPFILE $2
}

alias mv="mv -i"
alias cp="cp -i"
alias ls="ls --color=always"
alias less="less -r"
alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"

alias subl=subl3
