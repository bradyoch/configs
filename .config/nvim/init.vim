if has("win32")
  call plug#begin('~/AppData/Local/nvim/plugged')
else
  call plug#begin('~/.local/share/nvim/plugged')
endif

  Plug 'itchyny/lightline.vim'

  Plug 'scrooloose/nerdtree'

  Plug 'godlygeek/tabular'

  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-surround'

  Plug 'Raimondi/delimitMate'

  Plug 'bronson/vim-trailing-whitespace'

  Plug 'morhetz/gruvbox'
  Plug 'arcticicestudio/nord-vim'

  Plug 'dylon/vim-antlr'
  Plug 'ElmCast/elm-vim'

  Plug 'w0rp/ale'

call plug#end()

au BufRead, BufNewFile *.g set filetype=antlr3
au BufRead, BufNewFile *.g4 set filetype=antlr4

let g:mapleader = "\<space>"

nnoremap <leader>w <c-w>
nnoremap <silent> <F8> :NERDTreeToggle<cr>

let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ }

set nocompatible

syntax enable
filetype plugin indent on
set mouse=a

set termguicolors

colo gruvbox
set background=dark

set hidden

set number
set linebreak
set showmatch
set novisualbell

set cursorline
set colorcolumn=80

set nohlsearch
set smartcase
set ignorecase
set incsearch

set autoindent
set expandtab
set shiftwidth=2
set smarttab
set softtabstop=2
set tabstop=2

set ruler
set ff=unix

set scrolloff=4

set undolevels=1000
set backspace=indent,eol,start
