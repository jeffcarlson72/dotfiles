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
if [ "`type -t dir`" == "file" ] ; then
    alias dir='dir --group-directories-first --color=auto'
    alias vdir='vdir --group-directories-first --color=auto'
fi

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
fi

dox()
{
    type -p fzf >/dev/null || return

    doc=$( find /usr/share/doc -type f | sort | fzf )
    case $( basename "$doc" ) in
        *.gif|*.ico|*.jpg|*.jpeg|*.png)
            display "$doc" &
            ;;
        *.htm|*.html)
            lynx "$doc"
            ;;
        *.pdf)
            atril "$doc" &
            ;;
        *)
            less "$doc"
            ;;
    esac
    unset doc
}

h()
{
    type -p fzf >/dev/null || return

    $( history | awk '{$1 = ""; print}' | fzf )
}

logs()
{
    type -p fzf >/dev/null || return
    clrz=$( type -p colorize || type -p ccze ) || return

    sudo find /var/log/ -type f ! -name wtmp ! -name btmp |
        fzf |
        xargs -i sudo cat {} |
        $clrz |
        less -MR
}

pyle()
{
    type -p pygmentize >/dev/null || return

    pygmentize -g $1 | less -MR
}
