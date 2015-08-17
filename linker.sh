#!/bin/sh
# Linker.sh: link stuff from in here to "out there."
# (it's simpler than install.sh.)

doLink() {
  if [[ $1 = "-r" ]]; then
    EXEC="sudo";
    shift 1;
  else
    EXEC="exec";
  fi
    local dir="${0%/*}"
    local myCopy="$dir/$1";
    local sysCopy="$2";
    local oldConfigs="$HOME/.config/old-configs/"

    if [[ ! -f $myCopy ]]; then
        echo "File '$myCopy' doesn't exist!"
        exit 1
    fi

    if [[ -f $sysCopy ]]; then
        mkdir -p $oldConfigs;
        $EXEC mv $sysCopy $oldConfigs;
    elif [[ -L $sysCopy ]]; then
        $EXEC rm $sysCopy;
    fi
    $EXEC ln -s $myCopy $sysCopy;
}
msgInstall() {
    if [[ -z $(which $1) ]]; then
        echo "Be sure to install `$1`!"
        return 1
    else
        return 0
    fi
}

# ~/.config directory
doLink "config" "$HOME/.config"

doLink "bashrc" "$HOME/.bashrc"
doLink "inputrc" "$HOME/.inputrc"

# vimrc
doLink "vimrc" "$HOME/.vimrc";
# initialize vim
mkdir -p $HOME/.vim/{plugged,swap,backup,autoload}/;
if [[ ! -f $HOME/.vim/autoload/plug.vim ]]; then
    curl -fLo $HOME/.vim/autoload/plug.vim \
        "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim";
fi
msgInstall "vim" && vim +PlugInstall +qall

doLink "tmux.conf" "$HOME/.tmux.conf";
msgInstall "tmux"

doLink "xmonad" "$HOME/.xmonad"
doLink "xmobarrc"  "$HOME/.xmobarrc";
doLink "ghci"      "$HOME/.ghci";
msgInstall "xmonad"
msgInstall "xmobar"
msgInstall "ghc"

doLink "agignore"  "$HOME/.agignore";
msgInstall "ag"

doLink -r "configuration.nix" "/etc/nixos/configuration.nix"
