echo "BASH PROFILE"

PS1="$USER:\W$ "

alias edw="(/Applications/Emacs.app/Contents/MacOS/Emacs --daemon &)"
alias ec='emacsclient -t $* > /dev/null &'
ecw() {
    (emacsclient -c $* &)
}


# Tab complete any previously used ssh servers
complete -W "$(echo `cat ~/.ssh/known_hosts | cut -f 1 -d ' ' | sed -e s/,.*//g | uniq | grep -v "\["`;)" ssh scp

# emacs Binary
em() { /Applications/Emacs.app/Contents/MacOS/Emacs $1 & }

vimtris() { netris -k "hjl k" ; }

rl() {
    # Reload the profile
    source ~/.bash_profile
}

# Quick lookup for telephone extensions
tel() {
    list=~/Dropbox/sirius/phone-extensions.txt
    if [ ! $1 ]
    then
        # Show all if no argument specified
        cat $list | less
    else
        # Use agrep if available
        type -p agrep &>/dev/null && grepper="agrep -1" || grepper="grep"
        echo "$grepper -i $* $list" | bash -;
    fi
}

halp() {
    emacsclient -e "(find-grep \"find ~/org/ -type f | xargs agrep -n $1\")"
    osascript -e 'tell application "Emacs" to activate'
}

# Paths
export INFOPATH=/Users/stevechallis/.emacs.d/el-get/org-mode/doc:/Applications/Emacs.app/Contents/Resources/info:/usr/local/share/info/:/usr/local/info/:/usr/local/gnu/info/:/usr/local/gnu/lib/info/:/usr/local/gnu/lib/emacs/info/:/usr/local/emacs/info/:/usr/local/lib/info/:/usr/local/lib/emacs/info/:/usr/share/info/:/usr/lib/info/
export PYTHONPATH=/Users/stevechallis/Documents/projects/webtools/spyre/lib:/Users/stevechallis/Documents/projects/webtools/spyrecms/lib:/Users/stevechallis/Documents/projects/pySight/lib:/Users/stevechallis/Documents/repos/mongoengine:${PYTHONPATH}
export PATH=/opt/local/bin:/opt/local/sbin:/Users/stevechallis/scripts:$PATH
export PGDATA=$HOME/db/pg-data
export PATH=$PATH:$HOME/eev

# Virtualenv
#source /usr/local/bin/virtualenvwrapper.sh
export WORKON_HOME=/Users/stevechallis/envs
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
alias apod='cd /Users/stevechallis/Pictures/Wallpapers/apod'
alias arcon='pushd /usr/share/projects/arcon/src'
alias jxplorer='cd /Applications/jxplorer/; ./jxplorer.sh; cd -'
alias dropbox='cd ~/Dropbox'
alias sirius='cd ~/Documents/sirius'
alias cv='cd ~/Dropbox/documents/cv'
alias fyp='cd ~/Dropbox/FYP'
alias cms='cd ~/Documents/projects/webtools/spyrecms/'
alias repos='cd ~/Documents/repos/'
alias sqlite='sqlite3'
alias spyre='cd ~/Documents/projects/webtools/spyre'
alias spyrecms='cd ~/Documents/projects/webtools/spyrecms'
alias webtools='cd ~/Documents/projects/webtools'
alias documents='cd /Users/stevechallis/Documents'
alias movies='cd /Users/stevechallis/Movies'
alias pictures='cd /Users/stevechallis/Pictures'
alias music='cd /Users/stevechallis/Music'
alias library='cd /Users/stevechallis/Library'
alias downloads='cd /Users/stevechallis/Downloads'
alias desktop='cd /Users/stevechallis/Desktop'
alias projects='cd /Users/stevechallis/Documents/projects'
alias uni='cd /Users/stevechallis/Documents/uni'
alias sql='mysql -u root -p'
alias traction='cd /Users/stevechallis/Documents/tractiondigital'

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

export PATH="~/.lein/bin/:/usr/local/bin:/usr/local/sbin":$PATH

parents(){ :(){                                                                                                                       
          read p n <<<`ps -o ppid,comm -p $1 | awk 'NR>1{print $1,$2}'`;
          echo -e "$1\t$n";
          test $1 -ne 0 && : $p; };
      : $1; }

