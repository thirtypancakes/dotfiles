" Vi compatibilty is shit
set nocompatible

" Vundle
set rtp+=~/.vim/bundle/vundle/
" set rtp+=/usr/lib/python2.7/site-packages/powerline/bindings/vim
set rtp+=/usr/lib/python2.7/site-packages/powe
call vundle#rc()
Bundle "gmarik/vundle"

filetype off

" Bundle 'jellybeans.vim'
" Bundle 'croaker/mustang-vim'
Bundle 'houtsnip/vim-emacscommandline'
Bundle 'SirVer/ultisnips'
" Bundle 'vim-scripts/Vim-R-plugin'
Bundle "LaTeX-Box-Team/LaTeX-Box"
" Bundle "skwp/vim-colors-solarized"
" Bundle "majutsushi/tagbar.git"
Bundle "airblade/vim-gitgutter.git"
Bundle "tomtom/tcomment_vim.git"
" Bundle "bling/vim-airline"
" Bundle "fweep/vim-tabber"
Bundle "bling/vim-airline"
Bundle "Valloric/YouCompleteMe"

filetype plugin indent on

" Misc
set nowrap
set noshowmode
set relativenumber
set number
set backspace=indent,eol,start
set history=1000
set showcmd
set showmode
set gcr=a:blinkon0
set visualbell
set autoread

" Leader key
let mapleader=","
let maplocalleader=","

" Search settings
set incsearch
set hlsearch

" Backup stuff
set noswapfile
set nobackup
set nowritebackup

" Indenting and tabs
set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

" For airline
set laststatus=2

" Colourscheme
syntax on
colorscheme molokai

" Airline
let g:airline_powerline_fonts = 1
let g:airline_theme = "powerlineish"
let g:airline#extensions#bufferline#overwrite_variables = 0

" let g:bufferline_active_buffer_left = ' '
" let g:bufferline_active_buffer_right= ''

highlight bufferline_selected ctermfg=148 gui=bold cterm=bold term=bold
highlight link bufferline_selected_inactive airline_c_inactive
let g:bufferline_inactive_highlight = 'airline_c'
let g:bufferline_active_highlight = 'bufferline_selected'
let g:bufferline_active_buffer_left = ''
let g:bufferline_active_buffer_right = ''
