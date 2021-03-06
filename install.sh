#!/bin/bash
# duckwork 'dots' install script

# Will move the files where they need to be, and install git for use with github

DIR="${0%/*}" # better than PWD -- it can be called from anywhere

git_setup() {
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
aur_install() {
    d=$HOME/dl/aur
    mkdir -p $d
    for p in ${@##-*}; do
        cd "$d"
        curl "https://aur.archlinux.org/packages/${p:0:2}/$p/$p.tar.gz" \
            | tar xz
        cd "$p"
        makepkg ${@##[^\-]*}
    done
}

if [[ "$1" == "-g" ]]; then # github setup requested
    echo "Will set up git."
    echo "---------------------------------"
    git_setup || (
        echo "Install git!";
        sudo pacman -S git;
    )
fi

if [[ -f $HOME/.vimrc ]]; then
    mkdir -p $HOME/.config/old-configs/
    mv $HOME/.vimrc $HOME/.config/old-configs/.vimrc
elif [[ -L $HOME/.vimrc ]]; then
    rm $HOME/.vimrc
fi
ln -s $PWD/vimrc $HOME/.vimrc             # soft links = best practice
sudo ln -s $PWD/vimrc /root/.vimrc        # add vimrc to root for ease
mkdir -p $HOME/.vim/{bundle,swap,backup}/ # make all .vim/ dirs

echo "Configuring ViM..."
if [[ ! -f $HOME/.vim/autoload/plug.vim ]]; then
    echo "Installing Vim-Plug..."
    mkdir -p $HOME/.vim/{plugged,autoload}
    curl -fLo $HOME/.vim/autoload/plug.vim \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
vim +PlugInstall +qall

echo "Setting console font to Lat2-Terminus16..."
sudo echo "FONT=Lat2-Terminus16" > /etc/vconsole.conf

echo "Installing zsh & tmux..."
sudo pacman -S zsh tmux
echo "Setting zsh as default shell..."
chsh -s $(which zsh) "$(whoami)"

echo "Installing zshrc..."
if [[ -f $HOME/.zshrc ]]; then
    mkdir -p $HOME/.config/old-configs/
    mv $HOME/.zshrc $HOME/.config/old-configs/zshrc
elif [[ -L $HOME/.zshrc ]]; then
    rm $HOME/.zshrc
fi
echo "Configuring zsh..."
if [[ ! -d $HOME/.oh-my-zsh ]]; then
    echo "Installing Oh-My-Zsh..."
    curl -L http://install.ohmyz.sh | sh
fi
ln -s $PWD/zshrc $HOME/.zshrc

echo "Installing color stuff..."
if [[ ! -f "/usr/lib/libstderred.so" ]]; then
    aur_install -si stderred-git
fi
if [[ ! -d $HOME/.oh-my-zsh/custom/plugins/ ]]; then
    git clone git://github.com/zsh-users/zsh-syntax-highlighting.git $HOME/.oh-my-zsh/custom/plugins/
fi

echo "Installing ranger & dependeds..."
pacman -S ranger libcaca atool highlight mediainfo

echo "Finished."
