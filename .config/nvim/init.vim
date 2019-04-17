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

"
" Visual Settings
"

set termguicolors

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
