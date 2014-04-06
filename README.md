dotfiles
========

Various configuaration files for archlinux.

#How to install

1. install (stow)[http://www.gnu.org/software/stow/]
2. clone the epository into ~/dotfiles
3. run `stow <package>` to symlink all configuration files for that package to $HOME (e.g. `stow compton`
will symlink compton.conf to ~/.config/compton.conf
