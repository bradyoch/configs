if has("win32")
	call plug#begin('~/AppData/Local/nvim/plugged')
else
	call plug#begin('~/.local/share/nvim/plugged')
endif

Plug 'itchyny/lightline.vim'

Plug 'scrooloose/nerdtree'

Plug 'godlygeek/tabular'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'

Plug 'bronson/vim-trailing-whitespace'

Plug 'morhetz/gruvbox'

Plug 'zig-lang/zig.vim'

call plug#end()

if $TERM == "xterm-termite" || $TERM == "st-256color"
  set termguicolors
endif

colo gruvbox
set background=dark

set nocompatible
syntax enable
filetype plugin indent on

set number
set linebreak
set showmatch
set novisualbell

set modeline

set cursorline

set nohlsearch
set smartcase
set ignorecase
set incsearch

set autoindent
set expandtab
set shiftwidth=2
set smarttab
set softtabstop=2

set ruler
set ff=unix

set scrolloff=2

set undolevels=1000
set backspace=indent,eol,start
