# Terminal color codes
export BOLD="\033[1m"
export GRAY="\033[1;30m"
export CYAN="\033[0;36m"
export GREEN="\033[0;32m"
export BROWN="\033[0;33m"C
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

pythonpathadd() {
    if [ -d "$1" ] && [[ ! $PYTHONPATH =~ (^|:)$1(:|$) ]]; then
        PYTHONPATH=$1:$PYTHONPATH
    fi
}

export WORDS="/usr/share/dict/words"
export ORG="$HOME/org"
export PHONE_LIST="$ORG/phone-list.org"
export REPOS="$HOME/Documents/repos"
export APPENGINE_HOME="$REPOS/appengine-java-sdk-1.5.2"
export BOOKS="$ORG/books"
export HOWTO="$ORG/how-to"
export DOC="$ORG/doc"

usa() {
    ssh -D 7654 test1
}

alias edw="(/Applications/Emacs.app/Contents/MacOS/Emacs --daemon &)"
alias ec='emacsclient -t $* > /dev/null &'
ecw() {
    (emacsclient -c $* &)
}

# emacs Binary
em() { /Applications/Emacs.app/Contents/MacOS/Emacs $1 & }

# Tab complete any previously used ssh servers
#complete -W "$(echo `cat ~/.ssh/known_hosts | cut -f 1 -d ' ' | sed -e s/,.*//g | uniq | grep -v "\["`;)" ssh scp

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
#PYTHONPATH="$(printf "%s:" $REPOS/*)"
#export PYTHONPATH="${PYTHONPATH%:}"
#export PYTHONPATH=.:$PYTHONPATH:$REPOS/fido_platform/fido/

# These should be in the postactivate hook for virtualenv
#export PYTHONPATH=.:$REPOS/zonza/portal/:$REPOS/zonza/:$PYTHONPATH
#export PYTHONPATH=.:$REPOS/fido_platform/fido/:$REPOS/fido_platform/:$PYTHONPATH

DJANGO_SETTINGS_MODULE=portal.settings.local

# Virtualenv
source /usr/local/bin/virtualenvwrapper.sh
export WORKON_HOME=$HOME/envs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export VIRTUALENVWRAPPER_PYTHON='/Users/stevenchallis/envs/zonza/bin/python'

# Override standard commands
alias mkdir='mkdir -p'
alias curl='curl -O'
alias du='du -kh'
alias ls='ls -hFGC'
alias ll='ls -l'
if [ $TERM != "eterm-color" ]; then
    alias grep='GREP_COLOR="2;37;41" LANG=C grep --color=auto';
fi
alias grep='grep -n'
alias stat='stat -x'

alias p='pushd'
alias o='popd'

# Typos
alias xs='cd'
alias vf='cd'
alias co='cp'
alias ci='vi'
alias gvim='mvim'
alias bim='vim'
alias cim='vim'
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
alias fido='cd $REPOS/fido_platform'
alias zonza='cd $REPOS/zonza'
alias g='git'
alias gpr='git pull --rebase'
alias gsu='git submodule update'

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
alias tailf='tail -f'

export PATH="/Applications/NetBeans/NetBeans 6.9.1.app/Contents//Resources/NetBeans/java/ant/bin/ant":$PATH

export PATH="~/.lein/bin/:/usr/local/bin:/usr/local/sbin:$REPOS/leiningen/bin":$PATH

export PATH=$PATH:/usr/local/homebrew/bin:/usr/local/homebrew/sbin:/usr/local/pgsql/bin/

parents(){ :(){
          read p n <<<`ps -o ppid,comm -p $1 | awk 'NR>1{print $1,$2}'`;
          echo -e "$1\t$n";
          test $1 -ne 0 && : $p; };
      : $1; }


move_emails() {
    for f in `find . -type f -name "*.log" -d 1`; do
        echo $f | sed 's/\.log/\.eml/' | echo mv $f `awk "{print $1}"` | bash -;
    done;
}

#export NODE_PATH=/usr/local/homebrew/lib/node_modules
#PATH=/usr/local/homebrew/lib/node_modules/npm/node_modules/less/bin:$PATH

alias gvim='/Applications/MacVim.app/Contents/MacOS/Vim -g'

function check_keys() {
    # Check for my key
    ssh-add -l | grep ssh/steve>/dev/null
    if [ $? -gt 0 ]
    then
        echo -en $RED
        echo "You should add your key to the keychain"
        echo -en $NO_COLOUR
    fi
}
check_keys

export PIP_DOWNLOAD_CACHE=$HOME/.pip-cache

# AWS
alias eb="python2.7 $REPOS/AWS-ElasticBeanstalk-CLI-2.1/eb/macosx/python2.7/eb"
