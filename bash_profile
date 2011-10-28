echo "BASH PROFILE"

# Terminal color codes
export BOLD="\033[1m"
export GRAY="\033[1;30m"
export CYAN="\033[0;36m"
export GREEN="\033[0;32m"
export BROWN="\033[0;33m"
export RED="\033[0;31m"
export PURPLE="\033[0;35m"
export BLUE="\033[0;34m"
export YELLOW="\033[1;33m"
export NO_COLOUR="\033[0m"

source /usr/local/git/contrib/completion/git-completion.bash

#PS1="$USER:\W$ "
PS1='$(__git_ps1 "($GREEN%s$NO_COLOUR)")\h:\W\$ '

rl() {
    # Reload the profile
    source ~/.bash_profile
}

export WORDS="/usr/share/dict/words"
export ORG="$HOME/org"
export PHONE_LIST="$ORG/phone-list.org"
export REPOS="$HOME/Documents/repos"
export APPENGINE_HOME="$REPOS/appengine-java-sdk-1.5.2"
export BOOKS="$ORG/books"
export HOWTO="$ORG/how-to"
export DOC="$ORG/doc"

alias edw="(/Applications/Emacs.app/Contents/MacOS/Emacs --daemon &)"
alias ec='emacsclient -t $* > /dev/null &'
ecw() {
    (emacsclient -c $* &)
}

# emacs Binary
em() { /Applications/Emacs.app/Contents/MacOS/Emacs $1 & }

# Tab complete any previously used ssh servers
complete -W "$(echo `cat ~/.ssh/known_hosts | cut -f 1 -d ' ' | sed -e s/,.*//g | uniq | grep -v "\["`;)" ssh scp

vimtris() { netris -k "hjl k" ; }

# Quick lookup for telephone extensions
tel() {
    local NUMBER=$CYAN
    local TEXT=$GREEN
    if [ ! $1 ]
    then
        # Show all if no argument specified
        cat $PHONE_LIST | less
    else
        # Use agrep if available
        type -p agrep &>/dev/null && grepper="agrep -1" || grepper="grep"
        echo -en "$TEXT"
        echo -e "$grepper -i $* $PHONE_LIST |
            egrep '^[^\*].*' |
            sed 's/\([0-9]\{2,11\}x\{0,4\}\)/$NUMBER\1$TEXT/g'" |
            bash -
        echo -en "$NO_COLOUR"
    fi
}

halp() {
    emacsclient -e "(find-grep \"find ~/org/ -type f | xargs agrep -n $1\")"
    osascript -e 'tell application "Emacs" to activate'
}

# Paths
export INFOPATH=$HOME/.emacs.d/el-get/org-mode/doc:/Applications/Emacs.app/Contents/Resources/info:/usr/local/share/info/:/usr/local/info/:/usr/local/gnu/info/:/usr/local/gnu/lib/info/:/usr/local/gnu/lib/emacs/info/:/usr/local/emacs/info/:/usr/local/lib/info/:/usr/local/lib/emacs/info/:/usr/share/info/:/usr/lib/info/
export PATH=/opt/local/bin:/opt/local/sbin:$HOME/scripts:$PATH:$HOME/eev
export PATH=$PATH:$HOME/Documents/repos/appengine-java-sdk-1.5.2/bin
export PGDATA=$HOME/db/pg-data
export PYTHONPATH=$HOME/Documents/repos/mongoengine:${PYTHONPATH}

# Virtualenv
source /usr/local/bin/virtualenvwrapper.sh
export WORKON_HOME=$HOME/envs
export PIP_VIRTUALENV_BASE=$WORKON_HOME

# Override standard commands
alias mkdir='mkdir -p'
alias curl='curl -O'
alias du='du -kh'
alias ls='ls -hFGC'
alias ll='ls -l'
alias grep='GREP_COLOR="1;37;41" LANG=C grep --color=auto'
alias stat='stat -x'

alias p='pushd'
alias o='popd'

# Typos
alias xs='cd'
alias vf='cd'
alias co='cp'
alias ci='vi'
alias gvim='mvim'
alias gv='mvim'

# Current aliases
alias org='cd $ORG'
alias apod="cd $HOME/Pictures/Wallpapers/apod"
alias arcon='pushd /usr/share/projects/arcon/src'
alias jxplorer='cd /Applications/jxplorer/; ./jxplorer.sh; cd -'
alias dropbox='cd ~/Dropbox'
alias sirius='cd ~/Dropbox/sirius'
alias cv='cd ~/Dropbox/documents/cv'
alias fyp='cd ~/Dropbox/FYP'
alias cms='cd ~/Documents/projects/webtools/spyrecms/'
alias repos='cd ~/Documents/repos/'
alias sqlite='sqlite3'
alias spyre='cd ~/Documents/projects/webtools/spyre'
alias spyrecms='cd ~/Documents/projects/webtools/spyrecms'
alias webtools='cd ~/Documents/projects/webtools'
alias documents='cd $HOME/Documents'
alias movies='cd $HOME/Movies'
alias pictures='cd $HOME/Pictures'
alias music='cd $HOME/Music'
alias library='cd $HOME/Library'
alias downloads='cd $HOME/Downloads'
alias desktop='cd $HOME/Desktop'
alias projects='cd $HOME/Documents/projects'
alias uni='cd $HOME/Documents/uni'
alias sql='mysql -u root -p'
alias traction='cd $HOME/Documents/tractiondigital'
alias portal='cd $REPOS/zonza/portal'
alias zonza='cd $REPOS/zonza'
alias g='git'

alias finder='open -a finder .'
alias webserver='cd /Library/Webserver/Documents'
alias volumes='cd /Volumes'

# Scripts
alias getip='TEMP=$PWD; cd ~/scripts; ./getip; cd $TEMP'
alias pidof='TEMP=$PWD; cd ~/scripts; ./pidof $* ; cd $TEMP'
alias rpg='TEMP=$PWD; cd ~/scripts; ./rpg $* ; cd $TEMP'
alias gensha1='TEMP=$PWD; cd ~/scripts; ./gensha1 $* ; cd $TEMP'
alias genmd5='TEMP=$PWD; cd ~/scripts; ./genmd5 $* ; cd $TEMP'
alias preview='open /Applications/Preview.app $*'
alias linecount='grep -v ".svn" `find . -iname "*.py"` | wc -l'

export PATH="/Applications/NetBeans/NetBeans 6.9.1.app/Contents//Resources/NetBeans/java/ant/bin/ant":$PATH

export PATH="~/.lein/bin/:/usr/local/bin:/usr/local/sbin:$REPOS/leiningen/bin":$PATH

export PATH=$PATH:/usr/local/homebrew/bin

parents(){ :(){
          read p n <<<`ps -o ppid,comm -p $1 | awk 'NR>1{print $1,$2}'`;
          echo -e "$1\t$n";
          test $1 -ne 0 && : $p; };
      : $1; }

