#    _               _                           __ _ _      
#   | |__   __ _ ___| |__       _ __  _ __ ___  / _(_) | ___ 
#   | '_ \ / _` / __| '_ \     | '_ \| '__/ _ \| |_| | |/ _ \ 
#  _| |_) | (_| \__ \ | | |    | |_) | | | (_) |  _| | |  __/
# (_)_.__/ \__,_|___/_| |_|____| .__/|_|  \___/|_| |_|_|\___|
#                        |_____|_|                           

# Get the aliases and functions
if [ -f ~/.bashrc ] ; then
    . ~/.bashrc
fi

# User specific environment and startup programs

ANSIBLE_CONFIG=$HOME/.ansible/ansible.cfg
BASH_ENV=$HOME/.bashrc
CVS_RSH=ssh
HISTCONTROL=ignoredups
HISTFILESIZE=5000
HISTIGNORE=history
HISTSIZE=$HISTFILESIZE
LESS=-MR
PATH=$PATH:$HOME/bin

case `uname -s` in
    Darwin|FreeBSD|NetBSD)
	CLICOLOR=1
	;;
esac

if [ "`type -t emacs`" == 'file' ] ; then
    EDITOR=emacsclient
    if [ "`type -t mg`" == 'file' ] ; then
	ALTERNATE_EDITOR=mg
    elif [ "`type -t zile`" == 'file' ] ; then
	ALTERNATE_EDITOR=zile
    else
	ALTERNATE_EDITOR=emacs
    fi
elif [ "`type -t mg`" == 'file' ] ; then
    EDITOR=mg
elif [ "`type -t zile`" == 'file' ] ; then
    EDITOR=zile
fi

if [ -d $HOME/.bash_history ] ; then
    HISTFILE=$HOME/.bash_history/$( hostname -s )
fi

if [ $( locale -a | fgrep -q C.utf8 ) ] ; then
    LANG=C.utf8
else
    LANG=C
fi

export ALTERNATE_EDITOR ANSIBLE_CONFIG BASH_ENV CLICOLOR CVS_RSH EDITOR \
    HISTCONTROL HISTFILE HISTFILESIZE HISTIGNORE HISTSIZE LANG LESS PATH

if [ -d $HOME/.local/lib/bash ] ; then
    for i in $HOME/.local/lib/bash/*.sh ; do
	. $i
    done
fi

if [ -x /usr/games/fortune ] ; then
    /usr/games/fortune
elif [ -x /usr/bin/fortune ] ; then
    /usr/bin/fortune
fi
