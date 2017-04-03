echo "Installing zsh & emacs"
sudo apt-get install zsh -y
sudo apt-get install git -y
sudo apt-get install emacs -y
echo "[Done]"

echo "Installing oh-my-zsh"
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
echo "[Done]"

echo "Chaging to zsh"
chsh -s `which zsh`
echo "[Done]"


echo "Creating dotfiles symbolic links"
ZSH_DIR=$PWD/zshrc
EMACS_DIR=$PWD/emacs.d

echo "Creating zshrc symbolic link"
if [ -f $HOME/.zshrc ];then
  rm $HOME/.zshrc
fi
ln -s $ZSH_DIR $HOME/.zshrc
ln -s $PWD/aliases $HOME/.aliases
echo "[Done]"
echo "Creating emacs symbolic link"
if [ -d $HOME/.emacs.d ];then
  rm -rf $HOME/.emacs.d
fi
ln -s $EMACS_DIR $HOME/.emacs.d
echo "[Done]"
echo "Every link created correctly"
