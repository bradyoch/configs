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
endif

call plug#begin('~/.local/share/nvim/plugged')

  Plug 'junegunn/fzf.vim'

  Plug 'rakr/vim-one'
  Plug 'itchyny/lightline.vim'

  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-surround'

  Plug 'editorconfig/editorconfig-vim'

  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugin' }
  Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh'
        \ }

call plug#end()

let g:lightline = {
      \ 'colorscheme': 'one'
      \ }

let g:python3_host_prog = '~/.virtualenvs/pyls/bin/python'
let g:deoplete#enable_at_startup = 1

let g:LanguageClient_serverCommands = {
      \ 'python': ['~/.virtualenvs/pyls/bin/python', '-m', 'pyls']
      \ }

let $FZF_DEFAULT_COMMAND = 'fd --type f'

"
" Keymappings
"

nnoremap Q <Nop>
nnoremap Y y$

nnoremap <Leader>f :Files<CR>
nnoremap <Leader>r :Rg<CR>
nnoremap <Leader><Space> <C-w>

"
" Autocommands
"

augroup Vimrc
  au!
  autocmd BufWritePre * :call TrimTrailingWhitespace()
augroup END " Vimrc

"
" Visual Settings
"

if $TERM != 'linux'
  set termguicolors
endif
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
set inccommand=nosplit " show command effect as it is typed

set expandtab " use spaces
set tabstop=2 " 2 spaces
set shiftwidth=0 " sw = ts

set ruler " show line and col number

set updatetime=500

let g:tex_flavor = 'latex'

"
" Functions
"

function! TrimTrailingWhitespace()
  let l:save = getpos(".")
  %s/\s\+$//e
  :call setpos(".", l:save)
endfunction "TrimTrailingWhitespace
command! TrimWhitespace call TrimTrailingWhitespace()
