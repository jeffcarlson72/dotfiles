#    _               _
#   | |__   __ _ ___| |__  _ __ ___ 
#   | '_ \ / _` / __| '_ \| '__/ __|
#  _| |_) | (_| \__ \ | | | | | (__ 
# (_)_.__/ \__,_|___/_| |_|_|  \___|
#

# Source global definitions
if [ -f /etc/bashrc ] ; then
    # RH
    . /etc/bashrc
elif [ -f /etc/bash.bashrc ] ; then
    # Deb
    . /etc/bash.bashrc
fi

# User specific aliases and functions

[[ "$-" != *i* ]] && return

alias cp='cp -ip'
alias mv='mv -i'
alias rm='rm -i'
if [ `uname -s` == 'OpenBSD' -a -x '/usr/local/bin/colorls' ] ; then
    alias ls='colorls'
fi
if [ "`type -t dir`" == "file" ] ; then
    alias dir='dir --group-directories-first --color=auto'
    alias vdir='vdir --group-directories-first --color=auto'
else
    # In case of BSD, which does not have an option to list
    # directories first
    alias dir='ls'
    alias vdir='ls -l'
fi
alias kpcli="kpcli --kdb Passwords.kdbx --key Passwords.key"

shopt -s extglob      		# regex-like shell globs -- [!@*+?](pattern)
shopt -s lithist      		# whitespace in shell history
if [ ${BASH_VERSINFO[0]} -gt 3 ] ; then
    shopt -s autocd   		# cd into a dir as a command
    shopt -s globstar 		# expand /**/
fi

if [ -d $HOME/.local/lib/bash ] ; then
    for i in $HOME/.local/lib/bash/*.sh ; do
	. $i
    done
    unset i
fi

# Local Variables:
# mode: shell-script
# sh-shell: bash
# End:
