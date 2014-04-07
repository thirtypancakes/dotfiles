#dotfiles

Various configuration files for archlinux.

##How to install

1. install [stow](http://www.gnu.org/software/stow/)
2. clone the repository into `~/dotfiles`
3. run `stow <package>` to symlink all configuration files for that package
   into the relevant subdirectory in $HOME (e.g. `stow compton` will symlink
   `compton.conf` to `~/.config/compton.conf`

More information about using `stow` to manage your dotfiles can be found
[here](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html)
