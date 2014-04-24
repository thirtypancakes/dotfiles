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
Bundle 'bling/vim-bufferline'
Bundle 'bling/vim-airline'
Bundle 'junegunn/vim-easy-align'
Bundle 'airblade/vim-gitgutter'
Bundle 'tpope/vim-fugitive'
Bundle 'altercation/vim-colors-solarized'
Bundle 'emacscommandline'
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'plasticboy/vim-markdown'
Bundle 'isnowfy/python-vim-instant-markdown'
Bundle 'Shougo/neocomplete.vim'
Bundle 'terryma/vim-expand-region'
Bundle 'justinmk/vim-sneak'

filetype plugin indent on

" Sets
" misc
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

" completion
set wildmode=full
set wildmenu

" search settings
set incsearch
set hlsearch

" backup stuff
set noswapfile
set nobackup
set nowritebackup

" indenting and tabs
set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

" for airline
set laststatus=2

" colourscheme
set background=dark
syntax on
colorscheme solarized

" Lets
" airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#bufferline#overwrite_variables = 1
let g:airline_theme = 'solarizedish'

" highlight bufferline_selected ctermfg=3 gui=bold cterm=bold term=bold
" highlight link bufferline_selected_iairline_c_inactive
" let g:bufferline_inactive_highlight = 'Normal'
" let g:bufferline_show_bufnr = 0
" let g:bufferline_echo = 0

" ctrlp
let g:ctrlp_show_hidden = 1
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
let g:ctrlp_use_caching = 0

" easyalign
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

" neocomplete
let g:neocomplete#enable_at_startup = 1

" nerdtree
let g:NERDTreeShowHidden=1

" Some custom highlighting
hi VertSplit ctermfg=bg ctermbg=bg

" leader key
let mapleader="\<Space>"
let maplocalleader="\<Space>"

" easyalign
vmap <Enter> <Plug>(EasyAlign)
nmap <Leader>a <Plug>(EasyAlign)

" nerdtree
map <C-e> :NERDTreeToggle<CR>

" buffers
nmap <C-1> :bn<CR>
nmap <C-2> :bp<CR>

" ctrlp
map <C-f> :CtrlPLine<CR>
noremap <Leader>o :CtrlP<CR>

" better clipboard copy paste
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" sneek
nmap f <Plug>Sneak_s
nmap F <Plug>Sneak_S
xmap f <Plug>Sneak_s
xmap F <Plug>Sneak_S
omap f <Plug>Sneak_s
omap F <Plug>Sneak_S

" expand-region
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" neocomplete
inoremap <expr><C-g> neocomplete#undo_completion()
