#!/bin/sh
# duckwork 'dots' install script

# Will move the files where they need to be, and install git for use with github

github_setup() {
    echo -n "First name: "; read firstname
    echo -n "Last name:  "; read lastname
    echo -n "Email: "; read email
    echo "---------------------------------"

    $(which git) || return 1

    git config --global user.name "$firstname $lastname"
    git config --global user.email "$email"
    git config --global color.ui true
    git config --global push.default matching

}

if [[ "$1" == "-g" ]]; then # github setup requested
    echo "Will set up git."
    github_setup || echo "Install git!";
fi

#mkdir -p $HOME/.vim/bundle
#git clone https://github.com/gmarik/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim
#vim +PluginInstall +qall
#ln -s dots/vimrc ~/.vimrc # soft links = best practice
