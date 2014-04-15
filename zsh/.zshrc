TEXMFHOME=$HOME/.texmf
OSFONTDIR=${HOME}/.fonts:/usr/share/fonts//:/usr/share/texmf/fonts//

export TERM=xterm-256color
export EDITOR=vim

eval `dircolors /etc/dir_colors`

alias pacman='aura'

export LESS_TERMCAP_mb=$'\E[01;36m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;36;1;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
