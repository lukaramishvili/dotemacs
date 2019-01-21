
# ~/.bash_profile
# if [ -f ~/.bashrc ]; then
#     . ~/.bashrc
# fi

# ~/.bashrc
# if [ -f ~/dotemacs/.bashrc ]; then
#     source ~/dotemacs/.bashrc
# fi


export PATH=/usr/local/bin:~/.composer/vendor/bin:$PATH
#export PATH=/Applications/XAMPP/bin:$PATH

#PS1="\H:\W \u\$ "
#export PS1

# 2>/dev/null avoids "Identity added" message for every launch (and M-! output)
ssh-add ~/.ssh/multiple_id_rsa 2>/dev/null

ssh-add ~/Documents/luka/luka.ge/ssh/luka_ge_id_rsa 2>/dev/null
ssh-add ~/Documents/bookulus/ssh-key/bookulus.ge.id_rsa 2>/dev/null

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


f(){
    # -U doesn't work (supposed to skip .gitignore/ignore files)
    # ag respects .gitignore, but only from git root directory, so specifically exclude node_modules (throwing a lot of errors for deep paths)
    ag -R --ignore node_modules "$*" .
}

#cannot name it either s or st
vcs-status(){
    if [ -d ./.hg ]; then
        hg status
    else
        git status -s
    fi
}
vcs-log(){
    if [ -d ./.hg ]; then
        hg log
    else
        git log
    fi
}
vcs-diff(){
    if [ -d ./.hg ]; then
        hg diff
    else
        git diff
    fi
}
vcs-pull(){
    if [ -d ./.hg ]; then
        hg pull && hg update
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
        hg add .
        hg commit -m "$*"
    else
        git add .
        git commit -a -m "$*"
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
        hg push
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
        git push
        ssh root@luka.ge "cd /projects/lb && git pull"
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


# separate file for aliases
if [ -f ~/dotemacs/.bash_aliases ]; then
    source ~/dotemacs/.bash_aliases
fi


