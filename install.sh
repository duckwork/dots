#!/bin/sh
# duckwork 'dots' install script

# Will move the files where they need to be, and install git for use with github

github_setup() {
    echo -n "First name: "; read firstname
    echo -n "Last name:  "; read lastname
    echo -n "Email: "; read email
    echo "---------------------------------"
    echo -n "Github user:"; read githubuser
    echo -n "Password: "; read password

    $(which git) || return 1

    git config --global user.name "$firstname $lastname"
    git config --global user.email "$email"
    git config --global color.ui true
    git config --global push.default simple

}

if [[ "$1" == "-g" ]]; then # github setup requested
    echo "Will set up git."
    github_setup || echo "Install git!";
fi

ln -s dots/vimrc ~/.vimrc # soft links = best practice
