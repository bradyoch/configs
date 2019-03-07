#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases

source /usr/share/bash-completion/bash_completion
source /usr/share/doc/pkgfile/command-not-found.bash

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export PATH="$HOME/.local/bin:$PATH"
export CPATH="$HOME/.local/include:$CPATH"
export LIBRARY_PATH="$HOME/.local/lib:$LIBRARY_PATH"


