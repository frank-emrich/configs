#!/bin/bash

target="$PWD"

function abort {
  echo "$1"
  exit 1
}

function install_symlink {
  f="$1" #relative path inside of the configs directory
  home_f="$2" #absolute path of the file in the home directory
  target_f="${target}/${f}" #absolute path of file in the configs dir
  if [ -e "$home_f" ] ; then
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


function install_source {
  source_to="$target/$1"
  source_from="$2"

  source_line=". ${source_to}"
  if grep -q "${source_line}" "${source_from}"; then
      echo "File $source_to already sourced from $source_from"
  else
      echo "Appending  '$source_line' to $source_from"
      echo "$source_line" >> "$source_from"
  fi
}


install_symlink .emacs.d ~/.emacs.d
install_symlink powerline ~/.config/powerline
install_symlink tmux/.tmux.conf.local ~/.tmux.conf.local
install_symlink tmux/.tmux/.tmux.conf ~/.tmux.conf

install_source .aliases ~/.bash_aliases
install_source .bashrc_additions ~/.bashrc


which powerline-daemon > /dev/null || echo "powerline not installed, consider \"pip install powerline-status\""
