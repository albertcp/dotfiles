GIT_REPO=$PWD

#Checking if emacs.d directoy exists
if [ -d "emacs.d" ]; then
  echo "Removing previous emacs.d directory"
  rm -rf emacs.d
  echo "... done"
  sleep 2
fi

#Installing spacemacs
echo "Installing spacemacs repository..."
git clone https://github.com/syl20bnr/spacemacs ./emacs.d
sleep 2

#Creating spacemacs link
echo "Creating spacemacs links..."
cd $HOME
if [ -L ".emacs.d" ] || [ -d ".emacs.d" ]; then
  rm -rf .emacs.d
fi
ln -s $GIT_REPO/emacs.d ./.emacs.d
if [ -L ".spacemacs" ]; then
  rm .spacemacs
fi
ln -s $GIT_REPO/spacemacs ./.spacemacs
cd $GIT_REPO
echo "...done"

#Creating zsh links
echo "installing zsh"
sudo pacman -S zsh --noconfirm
echo "zsh installed"

echo "checkout links zsh"
echo "TODO::: LINKING ZSH"

echo "Installing Oh-my-zsh"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
echo "Oh-MY-ZSH installed"

echo "Installing autocomplete"
git clone git://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
echo "ZSH autosuggestions installed"


echo "Everything done!"

