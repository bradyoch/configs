"
" Basic Vim Settings
"
set nocompatible

syntax enable
filetype plugin indent on
set mouse=a

set undolevels=1000
set backspace=indent,eol,start

"
" Plugins
"

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')

  Plug 'rakr/vim-one'
  Plug 'itchyny/lightline.vim'

call plug#end()

let g:lightline = {
      \ 'colorscheme': 'one'
      \ }

"
" Visual Settings
"

set termguicolors
set background=dark
color one

set number " show line numbers
set linebreak " word break

set showmatch " show matching brackets

set novisualbell " don't beep

set cursorline " show current line
set colorcolumn=80 " show column 80 (long lines)

set nohlsearch " don't show all search results

set scrolloff=4 " start scrolling with 4 lines

"
" Other Settings
"

set hidden " allow unsaved buffers

set smartcase " ignore case in searches unless caps
set ignorecase  " otherwise ignore
set incsearch " search as I type

set expandtab " use spaces
set tabstop=2 " 2 spaces
set shiftwidth=0 " sw = ts

set ruler " show line and col number
