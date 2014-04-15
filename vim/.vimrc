" Vi ,fullcompatibilty is shit
set nocompatible

" Vundle
set rtp+=~/.vim/bundle/vundle/
set rtp+=~/.vim/bundle/my-airline/ " custom airline theme
call vundle#rc()
Bundle 'gmarik/vundle'

filetype off

Bundle 'houtsnip/vim-emacscommandline'
Bundle 'SirVer/ultisnips'
Bundle 'LaTeX-Box-Team/LaTeX-Box'
Bundle 'majutsushi/tagbar.git'
Bundle 'airblade/vim-gitgutter.git'
Bundle 'tomtom/tcomment_vim.git'
Bundle 'bling/vim-airline'
Bundle 'Valloric/YouCompleteMe'
Bundle 'junegunn/vim-easy-align'
Bundle 'airblade/vim-gitgutter'
Bundle 'tpope/vim-fugitive'
Bundle 'altercation/vim-colors-solarized'
Bundle 'emacscommandline'
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdtree'

filetype plugin indent on

" Misc
set autochdir
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

" Completion
set wildmode=full
set wildmenu

" Leader key
let mapleader=','
let maplocalleader=','

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
set background=dark
syntax on
colorscheme solarized

" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#bufferline#overwrite_variables = 1
let g:airline_theme = 'solarizedish'

" highlight bufferline_selected ctermfg=3 gui=bold cterm=bold term=bold
" highlight link bufferline_selected_iairline_c_inactive
" let g:bufferline_inactive_highlight = 'Normal'
" let g:bufferline_show_bufnr = 0
" let g:bufferline_echo = 0

vmap <Enter> <Plug>(EasyAlign)
nmap <Leader>a <Plug>(EasyAlign)

" ctrlp
let g:ctrlp_show_hidden = 1
map <C-f> :CtrlPLine<CR>

let g:easy_align_delimiters = {
      \ '>': { 'pattern': '>>\|=>\|>' },
      \ '/': { 'pattern': '//\+\|/\*\|\*/', 'ignore_groups': ['String'] },
      \ '#': { 'pattern': '#\+', 'ignore_groups': ['String'], 'delimiter_align': 'l' },
      \ ']': {
      \     'pattern':       '[[\]]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \ ')': {
      \     'pattern':       '[()]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \ 'd': {
      \     'pattern': ' \(\S\+\s*[;=]\)\@=',
      \     'left_margin': 0,
      \     'right_margin': 0
      \   }
      \ }

" nerdtree
map <C-e> :NERDTreeToggle<CR>

" buffers
nmap <C-1> :bn<CR>
nmap <C-2> :bp<CR>

" Some custom highlighting
hi VertSplit ctermfg=bg ctermbg=bg
