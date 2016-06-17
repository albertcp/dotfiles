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
#---------------------- End of lines added by compinstall -----------------------------#

#----------------------- Extra configuration ------------------------------------------#
autoload -U promptinit
promptinit

# This will set the default prompt to the walters theme
#prompt redhat 
#autocompletion with arrow-key driven interface
zstyle ':completion:*' menu select
#autocompletion of command line switches for aliases
setopt completealiases
#preventing duplicate lines in the history
setopt HIST_IGNORE_DUPS
#----------------------- end extra configuration -------------------------------------#

#---------------------------------CONFIGURATION---------------------------------------#
## BEGIN ALIASES [pendiente de ponerlo en 1 archivo independiente .zsh_alises]
alias montar-triqui='sshfs v130120@www.alumnos.fi.upm.es:.'
alias montar-raspi='sshfs pi@albertcp.no-ip.org:. ~/Escritorio/raspi-home'
alias emacs-nox='emacs -nw'
alias tiempo='curl -4 wttr.in'
alias tiempo-luna='curl -4 wttr.in/Moon'
## END ALIASES

## BEGIN  ENV VARIABLES
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk/
## END ENV VARIABLES

# mascara
umask 0037
#------------------------------- END CONFIGURATION -----------------------------------#

#------------------------------- START ANTIGEN ---------------------------------------#
export TERM='xterm-256color'
source /usr/share/zsh/scripts/antigen/antigen.zsh
#load the oh-my-zsh library
antigen use oh-my-zsh

antigen bundle git
antigen theme lukerandall
antigen apply
#------------------------------- End Antigen -----------------------------------------#

# zsh-autosuggestions
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
