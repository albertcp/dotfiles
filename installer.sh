echo "Creating dotfiles symbolic links"
ZSH_DIR=$(pwd)/zsh/.zshrc
EMACS_DIR=$(pwd)/emacs.d

echo "Creating zshrc symbolic link..."
ln -s $ZSH_DIR $HOME/.zshrc
echo "... Done"
echo "Creating emacs symbolic link..."
ln -s $EMACS_DIR $HOME/.emacs.d
echo "... Done"
echo "Every link created correctly"
