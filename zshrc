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
## END ALIASES

## BEGIN  ENV VARIABLES
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk/
export EDITOR='emacs -nw'
## END ENV VARIABLES

## mask
umask 0037

#---------------------------------- END CONFIGURATION --------------------------------#

#----------------------------------- START ANTIGEN -----------------------------------#
export TERM='xterm-256color'
source /usr/share/zsh/scripts/antigen/antigen.zsh
#load the oh-my-zsh library
antigen use oh-my-zsh

antigen bundle git
antigen theme lukerandall
antigen apply
#------------------------------------ End Antigen -----------------------------------#

#-------------------------------------- Addons --------------------------------------#
# zsh-autosuggestions
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
#------------------------------------- End Addons -----------------------------------#

#------------------------------------- AEROSTACK ------------------------------------#
source /opt/ros/jade/setup.zsh
export AEROSTACK_WORKSPACE=/home/alberto/workspace/ros/quadrotor_stack_catkin_ws
export AEROSTACK_STACK=/home/alberto/workspace/ros/quadrotor_stack_catkin_ws/src/quadrotor_stack
export DRONE_STACK=/home/alberto/workspace/ros/quadrotor_stack_catkin_ws/src/quadrotor_stack
source $AEROSTACK_WORKSPACE/devel/setup.zsh
#------------------------------------ END AEROSTACK ---------------------------------#