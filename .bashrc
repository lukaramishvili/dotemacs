
shopt -s expand_aliases

# add to ~/.bash_profile
# if [ -f ~/.bashrc ]; then
#     . ~/.bashrc
# fi

# add to ~/.bashrc
# if [ -f ~/dotemacs/.bashrc ]; then
#     source ~/dotemacs/.bashrc
# fi

# Replace utils with GNU versions (more features for e.g. ls, etc)
# https://stackoverflow.com/a/25455055
# brew install coreutils
# utils are prefixed with g*, e.g. sort => gsort
# Commands also provided by macOS have been installed with the prefix "g".
# If you need to use these commands with their normal names, you
# can add a "gnubin" directory to your PATH from your bashrc like:
# PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

# add mysql to PATH
PATH="/usr/local/mysql/bin:$PATH"

# add rabbitmq to PATH
PATH="/usr/local/opt/rabbitmq/sbin:$PATH"


# autocomplete symlinked directories, add trailing slash, make completion case insensitive
# If ~/.inputrc doesn't exist yet: First include the original /etc/inputrc
# so it won't get overriden
if [ ! -a ~/.inputrc ]; then echo '$include /etc/inputrc
set completion-ignore-case On
set mark-symlinked-directories on' > ~/.inputrc; fi


# add Emscripten to PATH
# Outputs results when called. Who wrote this?!
# source /code/emsdk/emsdk_env.sh
source /code/emsdk/emsdk_env.sh > /dev/null

if [ ! -d ~/z.lua ]; then
  git clone https://github.com/skywind3000/z.lua ~/z.lua
fi


# download leiningen install script
if [ ! -e /usr/local/bin/lein ]; then
  wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -O /usr/local/bin/lein
  chmod +x /usr/local/bin/lein
  # install it
  lein
fi


# easier to remember
alias clipboard="pbcopy"

# set $_ZL_CMD in .bashrc/.zshrc to change the command (default z).
# set $_ZL_DATA in .bashrc/.zshrc to change the datafile (default ~/.zlua).
# set $_ZL_NO_PROMPT_COMMAND if you're handling PROMPT_COMMAND yourself.
# set $_ZL_EXCLUDE_DIRS to a comma separated list of dirs to exclude.
# set $_ZL_ADD_ONCE to '1' to update database only if $PWD changed.
# set $_ZL_MAXAGE to define a aging threshold (default is 5000).
# set $_ZL_CD to specify your own cd command.
# set $_ZL_ECHO to 1 to display new directory name after cd.
# set $_ZL_MATCH_MODE to 1 to enable enhanced matching.
# set $_ZL_NO_CHECK to 1 to disable path validation, use z --purge to clean
#
# some vars won't work without export (e.g. _ZL_DATA)
#
export _ZL_CMD="d" # "cd" breaks things, e.g. ruby gem
export _ZL_DATA="~/dotemacs/.zlua"
export _ZL_ECHO=1
eval "$(lua ~/z.lua/z.lua --init bash enhanced once fzf)"

# keep original cd
cdd(){
  builtin cd "$*"
}

# to disable Chromium api keys warning (methods that didn't work: .profile, .bashrc, Chromium.app>Info.plist>LSEnvironment):
# https://gist.github.com/ezeeyahoo/dc4bdd250c6c6468959e107ddaef53f4


# launch Firefox's Profile Manager to enable privacy-conscious profile from ffprofile.com
# /Applications/Firefox.app/Contents/MacOS/firefox -no-remote -ProfileManager

# import .csv passwords to Firefox (after closing Firefox):
# ffpass import --from PASSWORDSFILE.csv -d ~/Library/Application\ Support/Firefox/Profiles/FF_PROFILE_NAME

# fix non-retina screen subpixel antialiasing on Mojave
# defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO
# enable "Use font smoothing when available" in Preferences > General

# fix Preview not remembering open files
defaults write com.apple.Preview NSQuitAlwaysKeepsWindows -bool true

# ensure an appropriate bash version is installed (for associative arrays, etc) -- from https://clubmate.fi/upgrade-to-bash-4-in-mac-os-x/
# brew install bash
# Add the new shell to the list of allowed shells
# sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
# Change to the new shell
# chsh -s /usr/local/bin/bash

# for gitdiff
# brew install diff-so-fancy

# for touchbar/command-line keyboard switcher:
# git clone https://github.com/myshov/xkbswitch-macosx
# ln -s /Users/luka/dotemacs/xkbswitch-macosx/bin/xkbswitch /usr/local/bin

# don't forget to apply Karabiner-Elements configuration (to be usable before login) - Karabiner Preferences > Misc Tab > System Default Configuration > * Copy the current configuration...
# in the same tab, disable Show icon in menu bar

export PATH=/usr/local/bin:~/.composer/vendor/bin:$PATH
#export PATH=/Applications/XAMPP/bin:$PATH

#PS1="\H:\W \u\$ "
#export PS1

# to enable QuickLook of text files without an extension (e.g. LICENSE, README, Makefile):
# http://whomwah.github.io/qlstephen/
# extract this file to /Library/QuickLook/ and run $ qlmanage -r

# to enable QuickLook for all text files:
# https://gregbrown.co/code/typescript-quicklook
# 1. brew cask install qlcolorcode
# 2. find out content type of specific extension:
# mdls -name kMDItemContentType file-with-extension.lisp
# 3. add that to LSItemContentTypes array in ~/Library/QuickLook/QLColorCode.qlgenerator/Contents/Info.plist


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
    sleep 1.5
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

# compile C directly to wasm:
# https://dassur.ma/things/c-to-webassembly/
# and run it with this:
# <!DOCTYPE html>

# <script type="module">
# async function init() {
#     const { instance } = await WebAssembly.instantiateStreaming(
#         fetch("./output.wasm")
#     );
#     console.log(instance.exports.add(4, 1));
# }
# init();
# </script>
# --nostdlib -- Don’t try and link against a standard library
# -Wl,--no-entry,-Wl,--export-all \ # Flags passed to the linker
c2wasm(){
    NAME=`echo "$*" | cut -d'.' -f1`
    EXTENSION=`echo "$*" | cut -d'.' -f2`
    clang --target=wasm32 -nostdlib -Wl,--no-entry -Wl,--export-all -o "${NAME}.wasm" "$*"
}

c2html(){
    emcc "$*" -s WASM=1 -o "${NAME}.html"    
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
vcs-checkout-master(){
    git checkout master
}
gcm(){
    vcs-checkout-master
}
# deploy - git push and update to server
deploy(){
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
    elif [ $(pwd) = "/projects/lb-bulk" ]
    then
        git push tfs master
        npm run build
        perl -i -pe 's|<base href="/" />|<base href="/lb-bulk/" />|g' dist/lb-bulk/index.html
        rsync -r -v -e ssh /projects/lb-bulk/dist root@luka.ge:/projects/lb-bulk/
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
    deploy # deploy - will update server code on specific projects; git push otherwise
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


# disable auto-updates and auto-started services, even though they are disabled from their respective preferences.

# echo "" > /Users/luka/Library/LaunchAgents/com.valvesoftware.steamclean.plist
# echo "" > /Users/luka/Library/LaunchAgents/com.google.keystone.agent.plist
# echo "" > /Users/luka/Library/LaunchAgents/com.adobe.GC.Invoker-1.0.plist
# echo "" > /Library/LaunchAgents/com.adobe.AdobeCreativeCloud.plist
# echo "" > /Library/LaunchAgents/com.adobe.GC.AGM.plist
# echo "" > /Library/LaunchAgents/com.adobe.GC.Invoker-1.0.plist
# echo "" > /Library/LaunchAgents/com.microsoft.update.agent.plist
# echo "" > /Library/LaunchDaemons/com.adobe.acc.installer.v2.plist
# echo "" > /Library/LaunchDaemons/com.adobe.agsservice.plist
# echo "" > /Library/LaunchDaemons/com.microsoft.autoupdate.helper.plist
# chmod ugo-rwx /Users/luka/Library/LaunchAgents/com.valvesoftware.steamclean.plist
# chmod ugo-rwx /Users/luka/Library/LaunchAgents/com.google.keystone.agent.plist
# chmod ugo-rwx /Users/luka/Library/LaunchAgents/com.adobe.GC.Invoker-1.0.plist
# chmod ugo-rwx /Library/LaunchAgents/com.adobe.AdobeCreativeCloud.plist
# chmod ugo-rwx /Library/LaunchAgents/com.adobe.GC.AGM.plist
# chmod ugo-rwx /Library/LaunchAgents/com.adobe.GC.Invoker-1.0.plist
# chmod ugo-rwx /Library/LaunchAgents/com.microsoft.update.agent.plist
# chmod ugo-rwx /Library/LaunchDaemons/com.adobe.acc.installer.v2.plist 
# chmod ugo-rwx /Library/LaunchDaemons/com.adobe.agsservice.plist
# chmod ugo-rwx /Library/LaunchDaemons/com.microsoft.autoupdate.helper.plist





# separate file for aliases
if [ -f ~/dotemacs/.bash_aliases ]; then
    source ~/dotemacs/.bash_aliases
fi


