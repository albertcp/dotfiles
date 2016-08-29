echo "Creating dotfiles symbolic links"
ZSH_DIR=$(pwd)
EMACS_DIR=$(pwd)/emacs.d

echo "Creating zshrc symbolic link..."
echo "Testing if it's already install"
cd $HOME
if [ -L ".zshrc" ]; then
    echo "Removing previous symbolic links"
    rm ".zshrc"
fi
cd $ZSH_DIR
ln -s $ZSH_DIR $HOME/.zshrc
echo "... Done"
echo "Creating emacs symbolic link..."
ln -s $EMACS_DIR $HOME/.emacs.d
echo "... Done"
echo "Every link created correctly"
