# ~/.profile is not read by bash if ~/.bash_profile or ~/.bash_login exists.
# So we read ~/.profile explicitly.
[[ -e ~/.profile ]] && source ~/.profile
# In OSX interactive shells are ran as login shells by default.
# Bash doesn't read ~/.bashrc if it has run as a login shell.
# Many program assumes that their configurations are contained in ~/.bashrc
# So we read it also explicitly.
# For more detail story, visit below links:
# https://unix.stackexchange.com/a/119675
# https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html#Bash-Startup-Files
[[ -e ~/.bashrc  ]] && source ~/.bashrc
