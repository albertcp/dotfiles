#!/usr/bin/bash

$green='\e[0;31m'
$stop='\e[0m'

echo "Installing zsh & emacs"
sudo apt-get install zsh -y
sudo apt-get install git -y
sudo apt-get install emacs -y
echo -e "$green[Done]$stop"

echo "Installing oh-my-zsh"
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
echo -e "$green[Done]$stop"

echo "Chaging to zsh"
chsh -s `which zsh`
echo -e "$green[Done]$stop : A reboot should be done to activate zsh"


echo "Creating dotfiles symbolic links"
ZSH_DIR=$PWD/zshrc
EMACS_DIR=$PWD/emacs.d
ALIASES=$PWD/aliases
WORK_ALIASES=$PWD/aliases_work

echo "Creating zshrc symbolic link"
if [ -f $HOME/.zshrc ];then
  rm $HOME/.zshrc
fi
ln -s $ZSH_DIR $HOME/.zshrc

if [ -f $HOME/.aliases ];then
    rm $HOME/.aliases
fi
ln -s $PWD/aliases $HOME/.aliases

if [ -f $HOME/.aliases_work ];then
    rm $HOME/.aliases_work
fi
ln -s $PWD/aliases_work $HOME/.aliases_work
echo -e "$green[Done]$stop"

echo "Creating emacs symbolic link"
if [ -d $HOME/.emacs.d ];then
  rm -rf $HOME/.emacs.d
fi
ln -s $EMACS_DIR $HOME/.emacs.d
echo -e "$green[Done]$stop"

echo "Installing zsh-autosuggestions & zsh-syntax-highlighting"
. ~/.zshrc
git clone git://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
echo -e "$green[Done]$stop"

echo -e "$green[Done]$stop : Dotfiles install"
