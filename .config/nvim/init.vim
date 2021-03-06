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

  Plug 'morhetz/gruvbox'
  Plug 'itchyny/lightline.vim'

  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-surround'
  Plug 'editorconfig/editorconfig-vim'

  Plug 'sheerun/vim-polyglot'

  Plug 'sbdchd/neoformat'
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh',
        \ }
  Plug 'junegunn/fzf.vim'

call plug#end()

let g:python3_host_prog = '/usr/bin/python3'

let g:deoplete#enable_at_startup = 1
let g:lightline = { 'colorscheme': 'gruvbox' }

let g:LanguageClient_serverCommands = {
      \ 'python': ['pyls'],
      \ 'typescript': ['typescript-language-server', '--stdio'],
      \ }

let g:neoformat_basic_format_align = 0
let g:neoformat_basic_format_retab = 0
let g:neoformat_basic_format_trim = 1

"
" Keybindings
"

let mapleader = ' '

nnoremap Y y$
nmap Q <Nop>
inoremap jf <Esc>

let $FZF_DEFAULT_COMMAND = 'fd --type f'
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>fr :Rg<CR>
nnoremap <Leader>w <C-w>

nmap <F5> <Plug>(lcn-menu)
nmap <silent>K <Plug>(lcn-hover)
nmap <silent> gd <Plug>(lcn-definition)
nmap <silent> <F2> <Plug>(lcn-rename)


"
" Autocommands
"

augroup Vimrc
  au!
  autocmd BufWritePre * undojoin | Neoformat
augroup END " Vimrc

"
" Visual Settings
"

if $TERM != 'linux'
  set termguicolors

  set background=dark
  colorscheme gruvbox
endif

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

" set foldmethod=syntax
set nofoldenable

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
