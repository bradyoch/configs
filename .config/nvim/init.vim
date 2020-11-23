" 
" Compatibility
"

" No vi compatibility mode
set nocompatible

" Syntax highlighting on
syntax enable
filetype plugin on

" Enable mouse
set mouse=a

" Misc
set undolevels=1000
set backspace=indent,eol,start

"
" Plugins
"

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin('~/.local/share/nvim/plugged')

  Plug 'junegunn/fzf.vim'

  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-surround'

call plug#end()

" Plugin Settings

let $FZF_DEFAULT_COMMAND = 'fd --type f'

"
" Key Mappings
"

" Rebind jf to escape
inoremap jf <ESC>

nnoremap Q <Nop>
nnoremap Y y$

nnoremap <Leader>f :Files<CR>
nnoremap <Leader>r :Rg<CR>
nnoremap <Leader><Space> <C-w>

"
" Visual Settings
"

" if $TERM != 'linux'
"   set termguicolors
" endif

set number " show line numbers
set linebreak " word break

set showmatch " show matching brackets

set novisualbell " don't beep

set scrolloff=4 " leave 4 line buffer when scrolling

"
" Other settings
"

set hidden " allow unsaved buffers

set smartcase " ignore case in search unless search includes caps
set ignorecase " ignore case otherwise
set incsearch " search without pressing enter
set inccommand=nosplit " show changes in replacement live

set expandtab " use spaces
set tabstop=2 " 2 spaces
set shiftwidth=0 " sw = ts

set ruler "show line and column number

set updatetime=500
