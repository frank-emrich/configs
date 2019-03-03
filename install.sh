#!/bin/bash

target="$PWD"

function abort {
  echo "$1"
  exit 1
}

function install {
  f="$1" #relative path inside of the configs directory
  home_f="$2" #absolute path of the file in the home directory
  target_f="${target}/${f}" #absolute path of file in the configs dir
  if [ -f "$home_f" ] ; then
    if [ -L "$home_f" ] ; then
      existing_link_target="$(readlink -f $home_f)"
      if [ "$existing_link_target" = "$target_f" ] ; then
        echo "Right symlink for $f exists"
      else
        abort "Existing symlink $home_f, pointing somewhere else"
      fi
    else
      abort "Existing non-symlink file $home_f"
    fi
  else
    echo "Creating symlink from $home_f to $target_f"
    ln -s  "$target_f" "$home_f"
  fi

}


install .bash_aliases ~/.bash_aliases
install tmux/.tmux.conf.local ~/.tmux.conf.local
install tmux/.tmux/.tmux.conf ~/.tmux.conf

actual_bashrc_end="$(tail -n 1 ~/.bashrc)"
expected_bashrc_end=". $target/bashrc_footer"
if [ "$actual_bashrc_end" = "$expected_bashrc_end" ] ; then
  echo "bashrc_footer already installed"
else
  echo "appending $expected_bashrc_end to ~/.bashrc"
  echo "$expected_bashrc_end" >> ~/.bashrc
fi
