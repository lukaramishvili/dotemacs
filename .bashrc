
shopt -s expand_aliases

# ~/.bash_profile
# if [ -f ~/.bashrc ]; then
#     . ~/.bashrc
# fi

# ~/.bashrc
# if [ -f ~/dotemacs/.bashrc ]; then
#     source ~/dotemacs/.bashrc
# fi

# ensure an appropriate bash version is installed (for associative arrays, etc) -- from https://clubmate.fi/upgrade-to-bash-4-in-mac-os-x/
# brew install bash
# Add the new shell to the list of allowed shells
# sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
# Change to the new shell
# chsh -s /usr/local/bin/bash

# for gitdiff
# brew install diff-so-fancy

export PATH=/usr/local/bin:~/.composer/vendor/bin:$PATH
#export PATH=/Applications/XAMPP/bin:$PATH

#PS1="\H:\W \u\$ "
#export PS1

# 2>/dev/null avoids "Identity added" message for every launch (and M-! output)
ssh-add ~/.ssh/multiple_id_rsa 2>/dev/null

ssh-add ~/Documents/luka/luka.ge/ssh/luka_ge_id_rsa 2>/dev/null
ssh-add ~/Documents/bookulus/ssh-key/bookulus.ge.id_rsa 2>/dev/null
ssh-add ~/Documents/lb/ssh/crmfrontend_id_rsa 2>/dev/null

# quickly copying the ssh key to the server:
# ssh-copy-id -i ~/Documents/lb/ssh/crmfrontend_id_rsa.pub Luka.Ramishvili@crmfrontend-dev.lb.ge

# macOS VPN deletes VPN password in the Keychain when connecting.
# even widely used solution of allowing configd access didn't work.
# use echo -e "PASSWORD\c" > .vpn_pass to avoid adding newline to the password file.
vpn-lb(){
    vpn_pass="`cat ~/dotemacs/.vpn_pass`"
    scutil --nc start "LB VPN"
    sleep 1
    osascript -e "tell application \"System Events\" to keystroke \"$vpn_pass\""
    osascript -e "tell application \"System Events\" to keystroke return"
}

# from https://github.com/joaomoreno/dotfiles/blob/master/.bashrc
# If not running interactively, don't do anything
# DONT ENABLE THIS - STOPS WORKING FROM APPLESCRIPT ETC
#case $- in
#    *i*) ;;
#    *) return;;
#esac

# don't put duplicate lines in the history.
# there's also an option to not put lines starting with space, ignorespace and ignoreboth for ignoring both.
# See bash(1) for more options
HISTCONTROL=ignoredups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
# not available by default on macOS
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable colors in ls
export CLICOLOR=1

# if [ -f ~/wp-completion.bash ]; then
#     source ~/wp-completion.bash
# fi

# ln -s /Users/luka/Library/Mobile\ Documents/ /icloud
# ln -s /Users/luka/Library/Mobile\ Documents/ ~/icloud
# cd /icloud
# ln -s com~apple~ScriptEditor2/Documents scripts
# ln -s com~apple~CloudDocs docs

# for mercurial keyring extension from https://www.mercurial-scm.org/wiki/KeyringExtension
# easy_install keyring
# easy_install mercurial_keyring
# put the following two lines (already did it in this directory) in .hgrc:
# [extensions]
# mercurial_keyring =
# then put the repo address with username embedded in [paths] like this:
# [paths]
# default = https://USERNAME@REPOSERVER.COM/USERNAME/PROJECTNAME
# e.g. default = https://lukaramishvili@bitbucket.org/lukaramishvili/dotemacs

transfer() {
    if [ $# -eq 0 ]; then
        echo -e "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md";
        return 1;
    fi 
    tmpfile=$( mktemp -t transferXXX );
    if tty -s; then
        basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g');
        curl --progress-bar --upload-file "$1" "https://transfer.sh/$basefile" >> $tmpfile;
    else curl --progress-bar --upload-file "-" "https://transfer.sh/$1" >> $tmpfile ;
    fi;
    cat $tmpfile;
    rm -f $tmpfile;
}

# a quick way to open websites, e.g. $ web youtube.com; to avoid typing $ open h t t p s : / / etc
web(){
    open "https://""$*"
}

f(){
    # -U doesn't work (supposed to skip .gitignore/ignore files)
    # ag respects .gitignore, but only from git root directory, so specifically exclude node_modules (throwing a lot of errors for deep paths)
    ag -R --ignore node_modules "$*" .
}

#cannot name it either s or st, so named it vcs-status and aliased s and st
vcs-status(){
    if [ -d ./.hg ]; then
        hg status 2>/dev/null
    else
        git status -s
    fi
}
vcs-log(){
    if [ -d ./.hg ]; then
        hg log 2>/dev/null
    else
        git log
    fi
}
vcs-diff(){
    if [ -d ./.hg ]; then
        hg diff 2>/dev/null
    else
        git diff
    fi
}
vcs-pull(){
    if [ -d ./.hg ]; then
        hg pull 2>/dev/null
        hg update 2>/dev/null
    else
        git pull
    fi
}
vcs-commit(){
    # make wrapping commit message in quotes unnecessary
    # (WARNING: doesn't work on (parens), only when escaped or, ironically, in quotes)
    # everything after $ qd ..., including spaces, will be passed
    # for reference: $@ would wrap each word (separated by space) in separate quotes
    # ..e.g.: if using $@, qd foo bar => cam "foo" "bar"
    if [ -d ./.hg ]; then
        hg add . 2>/dev/null
        hg commit -m "$*" 2>/dev/null
    else
        git add . 2>/dev/null
        git commit -a -m "$*" 2>/dev/null
    fi
}
# quick commit (cam stands for git commit -a -m )
cam(){
    vcs-commit "$*"
}
# deploy - git push and update to server
d(){
    if [ $(pwd) = "/Users/luka/dotemacs" ]
    then
        hg push 2>/dev/null
    elif [ $(pwd) = "/projects/wom" ]
    then
        # wom deploy
        git push publish
        # this will ignore html output
        # wget https://womanizor.com/deploy -O /dev/null
        # display html output and format newlines correctly ($ is needed for escaped characters to work)
        # actual newline inserted because macOS/bsd sed doesn't recognize newline characters
        wget -q -O - https://womanizor.com/deploy | sed $'s/<br>/\
/g'
    elif [ $(pwd) = "/projects/vtb" ] || [ $(pwd) = "/projects/vtb/Layout" ]
    then
        git push
        ssh root@luka.ge "cd /projects/vtb/Layout/ && git pull"
    elif [ $(pwd) = "/projects/don" ] || [ $(pwd) = "/projects/don/Layout" ]
    then
        git push
        ssh root@luka.ge "cd /projects/don/Layout/ && git pull"
    elif [ $(pwd) = "/projects/avea" ] || [ $(pwd) = "/projects/avea/assets" ]
    then
        git push
        ssh root@luka.ge "cd /projects/avea && git pull"
        rsync -avz /projects/avea/wwwroot/*.html root@luka.ge:/projects/avea/wwwroot/
    elif [ $(pwd) = "/projects/calo" ]
    then
        git push
        ssh root@luka.ge "cd /projects/calo && git pull"
        rsync -avz /projects/calo/*.html root@luka.ge:/projects/calo/
    elif [ $(pwd) = "/projects/lb" ]
    then
        git push tfs master
        ssh Luka.Ramishvili@crmfrontend-dev.lb.ge "cd /projects/lb && git pull"
        # git push
        #git push
        #ssh root@luka.ge "cd /projects/lb && git pull"
        #rsync -avz /projects/lb/dist/*.html root@luka.ge:/projects/lb/dist/
    elif [ $(pwd) = "/www/bookulus.ge" ] || [ $(pwd) = "/www/bookulus.ge/web" ]
    then
        git push
        # on Ubuntu server, use $ git config --global credential.helper store
        # (from a comment on https://stackoverflow.com/a/5343146/324220)
        ssh root@bookulus.ge "cd /projects/bookulus.ge && git pull"
    elif [ $(pwd) = "/projects/kt" ] || [ $(pwd) = "/projects/kt/Layout" ]
    then
        git push
        ssh root@luka.ge "cd /projects/kt && git pull"
    elif [ $(pwd) = "/projects/asb" ] || [ $(pwd) = "/projects/asb/Layout" ]
    then
        git push
        ssh root@luka.ge "cd /projects/asb && git pull"
    elif [ $(pwd) = "/projects/ald" ] || [ $(pwd) = "/projects/ald/layout" ]
    then
        git push
        ssh root@luka.ge "cd /projects/ald && git pull"
        #rsync -avz /projects/ald/dist/*.html root@luka.ge:/projects/ald/dist/
    elif [ $(pwd) = "/projects/cx/Solution/HelixCore.WebApp" ]
    then
        git push
        ssh root@luka.ge "cd /projects/cx && git pull"
        cd /projects/cx/Solution/HelixCore.WebApp
        rsync -avz /projects/cx/Solution/HelixCore.WebApp/*.html root@luka.ge:/projects/cx/Solution/HelixCore.WebApp/
        rsync -avz /projects/cx/Solution/HelixCore.WebApp/m/css/*.css root@luka.ge:/projects/cx/Solution/HelixCore.WebApp/m/css/
        rsync -avz /projects/cx/Solution/HelixCore.WebApp/m/js/*.min.js root@luka.ge:/projects/cx/Solution/HelixCore.WebApp/m/js/
    elif [ $(pwd) = "/projects/bt" ]
    then
        git push
        ssh root@luka.ge "cd /projects/bt && git pull"
        #rsync -avz /projects/bt/dist/*.html root@luka.ge:/projects/bt/dist/
    else
        git push
        # TODO other projects' deploy paths
    fi
           
}
# quick deploy (commits with text after command, pushes to git and updates server code)
qd(){
    # show status for info or debugging, if something goes wrong
    vcs-status
    # no parens without quotes - see comment about $* in cam()
    vcs-commit "$*"
    d # deploy - will update server code on specific projects; git push otherwise
    #
    if [ $(pwd) = "/projects/wom" ]; then
        git push # also push to alternate remote
    fi
}

dist-lb(){
    # a project here means specific internal project which uses a specific commit of this repo when releasing to prod
    DEFAULTWORKINGBRANCH=master
    # since this command jumps between git commits, only proceed when there's no changes in the current directory
    # $(s) is an alias to compact git status command vcs-status defined above
    if [ $(s | wc -l) = "0" ]; then
        # this is for generating multiple dist directories per project, each containing separate commits
        # declare -A COMMITS_PER_PROJECT=( [master]=master [crm]=master [urms]=master [ufe]=master )
        # for K in "${!COMMITS_PER_PROJECT[@]}"; do
        #     echo $K --- ${COMMITS_PER_PROJECT[$K]}
        # done
        # try reading from command-line arguments (e.g. `dist-lb crm master`)
        PROJECTNAME=$1
        COMMITNAME=$2
        # if no project name was passed, then ask the user
        if [ "$PROJECTNAME" == "" ]; then
            read -e -p "Enter the project name we're publishing (default crm): " -i "" PROJECTNAME
        fi
        # if, when asked, the user didn't specify the project name
        if [ "$PROJECTNAME" == "" ]; then
            # PROJECTNAME=${1:crm}
            PROJECTNAME=crm
        fi
        # if no commit name (hash) was passed, then ask the user
        if [ "$COMMITNAME" == "" ]; then
            read -e -p "Enter which commit to publish (defaults to latest commit to $DEFAULTWORKINGBRANCH): " -i "" COMMITNAME
        fi
        # if, when asked, the user didn't specify the commit
        if [ "$COMMITNAME" == "" ]; then
            # meaning checking out latest commit from the default working branch
            # COMMITNAME=${2:$DEFAULTWORKINGBRANCH}
            COMMITNAME=$DEFAULTWORKINGBRANCH
        fi
        # go to specific branch we want to publish
        git checkout "$COMMITNAME"
        DESTDIR="/dist/lb"
        DESTFILE="$DESTDIR/$PROJECTNAME.zip"
        # remove last generated distribution archive
        rm "$DESTFILE"
        # archive the directory (-X removes annoying MACOSX/.DSStore files)
        # NB: if we simply used zip ... dist/* without cd'ing, the zip folder would include the parent dist folder
        cd dist
        zip -r -X "$DESTFILE" ./*
        cd ..
        # open "$DESTDIR"
        # open Finder with the generated file preselected
        osascript -e "tell application \"Finder\"" -e activate -e "reveal POSIX file \"$DESTFILE\"" -e end tell
        # restore the directory state to latest commit
        git checkout "$DEFAULTWORKINGBRANCH"
        echo "OK"
    else
        echo "Please commit your changes and try again."
    fi
}

dist(){
    if [ $(pwd) = "/projects/lb" ]; then
        dist-lb $1 $2 # e.g. dist crm master, or dist crm COMMITHASH
    else
        echo "Please cd to project root dir and try again."
    fi
}

safari-history(){
    sqlite3 ~/Library/Safari/History.db 'select visit_time,title from history_visits order by visit_time desc;' \
        | while read i; do d="${i%%.*}"; echo "$(date -r $((d+978307200))) | ${i#*|}"; done \
        | head -n 50 | less
}

# separate file for aliases
if [ -f ~/dotemacs/.bash_aliases ]; then
    source ~/dotemacs/.bash_aliases
fi


