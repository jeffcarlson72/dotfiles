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

for LANG in $( locale -a | grep -w ^C ) ; do
    [ ${#LANG} -gt 1 ] && break
done

if mountpoint -q $HOME ; then
    # HOME directory is shared between other nodes
    XDG_CACHE_HOME=$HOME/.cache/${HOSTNAME%%.*}
    XDG_CONFIG_HOME=$HOME/.config/${HOSTNAME%%.*}
    XDG_DATA_HOME=$HOME/.local/share/${HOSTNAME%%.*}
    #XDG_RUNTIME_DIR set by pam_systemd
fi

export ALTERNATE_EDITOR ANSIBLE_CONFIG BASH_ENV CLICOLOR CVS_RSH EDITOR \
    HISTCONTROL HISTFILE HISTFILESIZE HISTIGNORE HISTSIZE LANG LESS PATH \
    XDG_CACHE_HOME XDG_CONFIG_HOME XDG_DATA_HOME XDG_RUNTIME_DIR

[ -d ${XDG_CACHE_HOME:-$HOME/.cache} ] ||
    mkdir ${XDG_CACHE_HOME:-$HOME/.cache}
[ -d ${XDG_CONFIG_HOME:-$HOME/.config} ] ||
    mkdir ${XDG_CONFIG_HOME:-$HOME/.config}
[ -d ${XDG_DATA_HOME:-$HOME/.local/share} ] ||
    mkdir ${XDG_DATA_HOME:-$HOME/.local/share}

if [ -d $HOME/.local/lib/bash ] ; then
    for i in $HOME/.local/lib/bash/*.sh ; do
	. $i
    done
    unset i
fi

if [ -x /usr/games/fortune ] ; then
    case `uname -s` in
	FreeBSD)
	    /usr/games/fortune freebsd-tips
	    ;;
	NetBSD)
	    /usr/games/fortune netbsd-tips
	    ;;
	*)
	    /usr/games/fortune
	    ;;
    esac
elif [ -x /usr/bin/fortune ] ; then
    /usr/bin/fortune
fi

# Local Variables:
# mode: shell-script
# sh-shell: bash
# End:
