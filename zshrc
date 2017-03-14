# ----------------------- Oh My Zsh ------------------------ #
# Path to your oh-my-zsh installation.
export ZSH=/home/alberto/.antigen/repos/https-COLON--SLASH--SLASH-github.com-SLASH-robbyrussell-SLASH-oh-my-zsh.git

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
#ZSH_THEME="risto"
ZSH_THEME="lukerandall"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=$ZSH

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# ---------------------- End oh my zsh confifg ------------- #

# --------------------- Lines configured by zsh-newuser-install -----------------------#
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt notify
unsetopt autocd
bindkey -e
# --------------------- End of lines configured by zsh-newuser-install ----------------#

#---------------------- The following lines were added by compinstall -----------------#
zstyle :compinstall filename '/home/albertcp/.zshrc'

autoload -Uz compinit
compinit
#-------------------------- End of lines added by compinstall ------------------------#

#------------------------------ Extra zsh configuration ------------------------------#
autoload -U promptinit
promptinit

#autocompletion with arrow-key driven interface
zstyle ':completion:*' menu select
#autocompletion of command line switches for aliases
setopt completealiases
#preventing duplicate lines in the history
setopt HIST_IGNORE_DUPS
#------------------------------ end extra zsh configuration --------------------------#

#-----------------------------------CONFIGURATION-------------------------------------#
## BEGIN ALIASES [pendiente de ponerlo en 1 archivo independiente .zsh_alises]
alias emacs-nox='emacs -nw'
alias weather='curl -4 wttr.in'
alias moon-weather='curl -4 wttr.in/Moon'
alias clr='clear'

alias todo="emacs ~/TODO.org &"

# Aerostack aliases
alias cmirony-error.txt='catkin_make -DCMAKE_EXPORT_COMPILE_COMMANDS=ON 2> error.txt'
alias cmirony='catkin_make -DCMAKE_EXPORT_COMPILE_COMMANDS=ON'
#alias cdwmirony="actual_dir=$PWD && cdw && cmirony && cd $actual"

alias cm='catkin_make'
alias cm-error.txt='catkin_make 2> error.txt' 
# END Aliases

function cds(){
  cd $AEROSTACK_STACK/$1
}

function cdw(){
  cd $AEROSTACK_WORKSPACE/$1
}

function cdexecutive(){
  cd $AEROSTACK_STACK/stack_devel/executive_system/$1
}

function cdaerostackmsgs(){
  cd $AEROSTACK_STACK/stack/common/aerostack_msgs/$1
}

function cdconfigs(){
  cd $AEROSTACK_STACK/configs/$1
}

function ckmirony(){
  actual=$PWD
  cdw
  echo $#
  if [ $# -gt 1 ]; then
     catkin_make $1 $2
  else
     catkin_make
  fi
  cd $actual
}

# auto-complete
_cds(){
  _path_files -W $AEROSTACK_STACK -/
}

_cdw(){
  _path_files -W $AEROSTACK_WORKSPACE -/
}

_cdexecutive(){
  _path_files -W $AEROSTACK_STACK/stack_devel/executive_system -/
}

_cdconfigs(){
  _path_files -W $AEROSTACK_STACK/configs -/
}

compdef _cds cds
compdef _cdw cdw
compdef _cdexecutive cdexecutive
compdef _cdconfigs cdconfigs

## END ALIASES

## BEGIN  ENV VARIABLES
export EDITOR='emacs -nw'
## END ENV VARIABLES

## mask
umask 0037

#---------------------------------- END CONFIGURATION --------------------------------#


#------------------------------------- AEROSTACK ------------------------------------#
export PATH=$PATH:$AEROSTACK_STACK/launchers

source /opt/ros/jade/setup.zsh
export AEROSTACK_WORKSPACE=/home/alberto/workspace/ros/quadrotor_stack_catkin_ws
export AEROSTACK_STACK=/home/alberto/workspace/ros/quadrotor_stack_catkin_ws/src/quadrotor_stack
export DRONE_STACK=/home/alberto/workspace/ros/quadrotor_stack_catkin_ws/src/quadrotor_stack
source $AEROSTACK_WORKSPACE/devel/setup.zsh
#------------------------------------ END AEROSTACK ---------------------------------#
