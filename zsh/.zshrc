# exports
export TERM=xterm-256color
export EDITOR=vim
export TEXMFHOME=$HOME/.texmf
export OSFONTDIR=${HOME}/.fonts:/usr/share/fonts//:/usr/share/texmf/fonts//

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# better dir colors
eval `dircolors /etc/dir_colors`

# aliases
alias pacman='aura'
alias s='sudo'
alias so='source'

# don't need to rehash after installation
# zstyle ":completion:*:commands" rehash 1

# better man colours
export LESS_TERMCAP_mb=$'\E[01;36m'
export LESS_TERMCAP_md=$'\E[01;36;1;74m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[38;5;246m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;146m'

vims(){
  vim --servername $1
}
