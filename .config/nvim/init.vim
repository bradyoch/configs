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

  Plug 'bronson/vim-trailing-whitespace'

  Plug 'morhetz/gruvbox'

  Plug 'Shougo/deoplete.nvim', { 'do' : 'UpdateRemotePlugins' }

call plug#end()

let g:deoplete#enable_at_startup = 1

set nocompatible

syntax enable
filetype plugin indent on
set mouse=a

let g:mapleader = "\<space>"

nnoremap <leader>w <c-w>
nnoremap <leader>ct :NERDTreeToggle<cr>

set termguicolors

set background=dark
colo gruvbox

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
