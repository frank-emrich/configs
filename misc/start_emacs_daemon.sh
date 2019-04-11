#!/bin/bash
export DISPLAY=":0"
export TMUX=1
. ~/.profile
emacs --daemon
