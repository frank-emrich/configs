#!/bin/bash


# TODO: add inclusion of ~/.config/configs/path_additions_login_only
target="$PWD"

script_path=$(dirname "$0" | xargs realpath )

if [[ "$target" != "$script_path" ]] ; then
    echo "Execute me from where I'm saved"
    exit 1
fi


RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[0;32m'
NC='\033[0m' # No Color

function abort {
  echo -e "${RED}$1${NC}"
  exit 1
}

function warn {
  echo -e "${ORANGE}$1${NC}"
}

function ok {
  echo -e "${GREEN}$1${NC}"
}

function install_symlink {
  f="$1" #relative path inside of the configs directory
  home_f="$2" #absolute path of the file in the home directory
  target_f="${target}/${f}" #absolute path of file in the configs dir
  if [ -e "$home_f" ] ; then
    if [ -L "$home_f" ] ; then
      existing_link_target="$(readlink -f $home_f)"
      if [ "$existing_link_target" = "$target_f" ] ; then
        ok "Right symlink for $f exists"
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

function append_line_if_not_present {
  line="$1"
  file="$2"



  if grep -q "${line}" "${file}"; then
      ok "Line $line already present in $file"
  else
      echo "Appending  '$line' to $file"
      echo "$line" >> "$file"
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
      ok "File $source_to already sourced from $source_from"
  else
      echo "Appending  '$source_line' to $source_from"
      echo "$source_line" >> "$source_from"
  fi
}

function suggest_source {
  source_to="$target/$1"
  source_from="$2"

  if [ ! -e "$source_from" ] ; then
      warn "File $source_from did not exist, consider adding it and sourcing $source_to from it"
  else
      source_line=". ${source_to}"
      if grep -q "${source_line}" "${source_from}"; then
	  ok "File $source_to already sourced from $source_from"
      else
	  warn "File $source_from exists, consider adding the following:"
	  echo "$source_line"
      fi
  fi
}


function create_redirect_file() {
  cat <<EOF > $1
# Generated in order to define the following:
export MY_DOTFILE_CONFIGS_DIR="$PWD"

source $2
EOF
}


create_redirect_file "zshrc_redirect" "${target}/zsh/zshrc_additions"
create_redirect_file "bashrc_redirect" "${target}/bash/bashrc_additions"

#install_symlink .emacs.d ~/.emacs.d
install_symlink .doom.d ~/.doom.d

install_symlink powerline ~/.config/powerline

install_symlink tmux/.tmux.conf.local ~/.tmux.conf.local
install_symlink tmux/.tmux/.tmux.conf ~/.tmux.conf

install_symlink .xbindkeysrc ~/.xbindkeysrc

install_source aliases ~/.bash_aliases
install_source "bashrc_redirect" ~/.bashrc

install_source "zshrc_redirect" ~/.zshrc
install_symlink "zsh/.zprofile" ~/.zprofile
suggest_source "zsh/load_zshrc_if_zsh" ~/.profile

which powerline-daemon > /dev/null || warn "powerline not installed, consider \"pip install powerline-status\""

#Spelling
install_symlink "aspell/.aspell.en.prepl" ~/.aspell.en.prepl
install_symlink "aspell/.aspell.en.pws" ~/.aspell.en.pws


#Somehow, the permissions of the submodules are messed up?
submods=()
submods+=("zsh/oh-my-zsh")
submods+=("zsh/oh-my-zsh-custom/plugins")
submods+=("zsh/oh-my-zsh-custom/plugins/zaw")
submods+=("zsh/oh-my-zsh-custom/plugins/zsh-autosuggestions")
submods+=("zsh/oh-my-zsh-custom/plugins/zsh-syntax-highlighting")

for f in ${submods[@]}; do
    chmod o-wx "$f"
done



#Emacs daemon
misc/create_systemd_file.sh "emacs.service" "$target/misc"

echo "created emacs.service file, consider installing it via"
echo "cp emacs.service ~/.config/systemd/user/ && systemctl --user enable emacs.service"

#Konsole
mkdir -p ~/.local/share/kxmlgui5/konsole
install_symlink konsole/share ~/.local/share/konsole
install_symlink konsole/sessionui.rc ~/.local/share/kxmlgui5/konsole/sessionui.rc
install_symlink konsole/konsoleui.rc ~/.local/share/kxmlgui5/konsole/konsoleui.rc

# SSH config
mkdir -p ~/.ssh/controlmasters
touch ~/.ssh/config
append_line_if_not_present "Include $target/ssh/shared.config" ~/.ssh/config
