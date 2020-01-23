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
HISTIGNORE=history
LESS=-MR
PATH=$PATH:$HOME/bin

if [ $( locale | grep -q utf8 ) ] ; then
    LANG=C.utf8
else
    LANG=C
fi

if [ -f /usr/bin/emacs -o -f /usr/local/bin/emacs ] ; then
    EDITOR=emacsclient
    if [ -f /usr/bin/mg -o -f /usr/local/bin/mg ] ; then
	ALTERNATE_EDITOR=mg
    elif [ -f /usr/bin/zile -o -f /usr/local/bin/zile ] ; then
	ALTERNATE_EDITOR=zile
    else
	ALTERNATE_EDITOR=emacs
    fi
elif [ -f /usr/bin/mg -o -f /usr/local/bin/mg ] ; then
    EDITOR=mg
elif [ -f /usr/bin/zile -o -f /usr/local/bin/zile ] ; then
    EDITOR=zile
fi

case `uname -s` in
    Darwin|FreeBSD|NetBSD)
	CLICOLOR=1
	;;
esac

export ALTERNATE_EDITOR ANSIBLE_CONFIG BASH_ENV CLICOLOR CVS_RSH EDITOR \
    HISTIGNORE LANG LESS PATH

unset TMOUT

if [ -x /usr/games/fortune ] ; then
    /usr/games/fortune
elif [ -x /usr/bin/fortune ] ; then
    /usr/bin/fortune
fi
