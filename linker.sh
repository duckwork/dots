#!/bin/bash
# Linker.sh: link stuff from in here to "out there."
# (it's simpler than install.sh.)

doLink() {
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
        mv $sysCopy $oldConfigs;
    elif [[ -L $sysCopy ]]; then
        rm $sysCopy;
    fi
    ln -s $myCopy $sysCopy;
}
msgInstall() {
    if [[ -z $(which $1) ]]; then
        echo "Be sure to install `$1`!"
        return 1
    else
        return 0
    fi
}

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

doLink "xmonad.hs" "$HOME/.xmonad/xmonad.hs";
doLink "xmobarrc"  "$HOME/.xmobarrc";
doLink "ghci"      "$HOME/.ghci";
msgInstall "xmonad"
msgInstall "xmobar"
msgInstall "ghc"

doLink "agignore"  "$HOME/.agignore";
msgInstall "ag"

# TODO: /etc/nixos/configuration.nix
