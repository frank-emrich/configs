#!/bin/bash


target="$PWD"

script_path=$(dirname "$0" | xargs realpath )

if [[ "$target" != "$script_path" ]] ; then
    echo "Execute me from where I'm saved"
    exit 1
fi




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

  if [ ! -e "$source_from" ] ; then
      echo "File $source_from did not exist, created it"
      touch "$source_from"
  fi

  source_line=". ${source_to}"
  if grep -q "${source_line}" "${source_from}"; then
      echo "File $source_to already sourced from $source_from"
  else
      echo "Appending  '$source_line' to $source_from"
      echo "$source_line" >> "$source_from"
  fi
}


function create_switch_file() {
  cat <<EOF > $1
MY_DOTFILE_CONFIGS_DIR="$PWD"
source $2
EOF
}


create_switch_file "zshrc_switch" "${target}/zsh/zshrc_additions"
create_switch_file "bashrc_switch" "${target}/bash/bashrc_additions"

install_symlink .emacs.d ~/.emacs.d
install_symlink powerline ~/.config/powerline
install_symlink tmux/.tmux.conf.local ~/.tmux.conf.local
install_symlink tmux/.tmux/.tmux.conf ~/.tmux.conf

install_source .aliases ~/.bash_aliases
install_source "bashrc_switch" ~/.bashrc

install_source "bashrc_switch" ~/.zshrc


which powerline-daemon > /dev/null || echo "powerline not installed, consider \"pip install powerline-status\""
