
# add to ~/.bash_profile
# if [ -f ~/.bashrc ]; then
#     . ~/.bashrc
# fi

# add to ~/.bashrc
# if [ -f ~/dotemacs/.bashrc ]; then
#     source ~/dotemacs/.bashrc
# fi

LC_ALL=en_US.UTF-8
LC_CTYPE=en_US.UTF-8

export PATH="~/bin::$PATH"

export PATH="/usr/local/sbin:$PATH"

export PATH="/opt/homebrew/bin:$PATH"


shopt -s expand_aliases


# # change shell to bash
# chsh -s /bin/bash
# # and for root
# sudo chsh -s /bin/bash
# disable zsh warning
export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -d /usr/local/graalvm ]; then
  export GRAALVM_HOME=/usr/local/graalvm
  export PATH=$GRAALVM_HOME/bin:$PATH
fi

if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . /Users/luka/.nix-profile/etc/profile.d/nix.sh
fi

# Replace utils with GNU versions (more features for e.g. ls, etc)
# https://stackoverflow.com/a/25455055
# brew install coreutils
# utils are prefixed with g*, e.g. sort => gsort
# Commands also provided by macOS have been installed with the prefix "g".
# If you need to use these commands with their normal names, you
# can add a "gnubin" directory to your PATH from your bashrc like:
# PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"


eval "$(direnv hook bash)"


# hide login banner in new shell windows
# touch ~/.hushlogin


PATH="/Users/luka/Library/Android/sdk/platform-tools:$PATH"

# only have java 8 enabled
# export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

##
## multiple versions of java
##
export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_11_HOME=$(/usr/libexec/java_home -v11)

alias java8='export JAVA_HOME=$JAVA_8_HOME'
alias java11='export JAVA_HOME=$JAVA_11_HOME'

# default to Java 11
java11

ANDROID_SDK_ROOT="/Users/luka/Library/Android/sdk"

# add mysql to PATH
#PATH="/usr/local/mysql/bin:$PATH"
export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/mysql@5.7/lib"
export CPPFLAGS="-I/usr/local/opt/mysql@5.7/include"
export PKG_CONFIG_PATH="/usr/local/opt/mysql@5.7/lib/pkgconfig"

# add dotfiles to PATH
PATH="~/dotemacs/bin:$PATH"

# add rabbitmq to PATH
PATH="/usr/local/opt/rabbitmq/sbin:$PATH"

# install sq (jq-like command-line SQL/CSV/XLSX client)
# brew tap neilotoole/sq
# brew install sq
# # add completion to bash (from `sq completion --help`)
# sq completion bash > /usr/local/etc/bash_completion.d/sq

# add package.json script completion to yarn
# https://github.com/romainberger/yarn-completion
# yarn global add yarn-completion
# wget https://raw.githubusercontent.com/romainberger/yarn-completion/master/yarn-completion.bash -O ~/dotemacs/yarn-completion.bash
if [ -f ~/dotemacs/yarn-completion.bash ]; then
    . ~/dotemacs/yarn-completion.bash
    __yarn_completion_complete k
fi


# Download a youtube playlist
# --download-archive keeps a list of downloaded files to avoid re-downloading when re-running the script
# -k would also keep the video file, but for hundreds of songs, takes a huge amount of space
# youtube-dl --download-archive downloaded.txt --no-post-overwrites --extract-audio --audio-format mp3 --audio-quality 0 --embed-thumbnail --add-metadata --metadata-from-title "%(artist)s - %(title)s" -i -o "%(title)s.%(ext)s" https://www.youtube.com/playlist?list=FLH-M2Z5bOm3uyfQuKQbqTaA


# autocomplete symlinked directories, add trailing slash, make completion case insensitive
# If ~/.inputrc doesn't exist yet: First include the original /etc/inputrc
# so it won't get overriden
if [ ! -a ~/.inputrc ]; then echo '$include /etc/inputrc
set completion-ignore-case On
set mark-symlinked-directories on' > ~/.inputrc; fi


# add Emscripten to PATH
# Outputs results when called. Who wrote this?!
# source /code/emsdk/emsdk_env.sh
# this was the correct one but commented
# source /code/emsdk/emsdk_env.sh > /dev/null

if [ ! -d ~/z.lua ]; then
  git clone https://github.com/skywind3000/z.lua ~/z.lua
fi

if [ ! -d /usr/local/bin ]; then
  echo "Asking to create /usr/local/bin"
  sudo mkdir /usr/local/bin
fi

# download leiningen install script
if [ ! -e /usr/local/bin/lein ]; then
  wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -O /usr/local/bin/lein
  chmod +x /usr/local/bin/lein
  # install it
  lein
fi


# Usage: mv oldfilename
# If you call mv without the second parameter it will prompt you to edit the filename on command line.
# Original mv is called when it's called with more than one argument.
# It's useful when you want to change just a few letters in a long name.
function mv() {
  if [ "$#" -ne 1 ]; then
    command mv "$@"
    return
  fi
  if [ ! -f "$1" ]; then
    command file "$@"
    return
  fi
  read -ei "$1" newfilename
  mv -v "$1" "$newfilename"
} 


# easier to remember
alias clipboard="pbcopy"


backup-mac-preferences(){
  # backup the app shortcuts preferences plist
  cp ~/Library/Preferences/.GlobalPreferences.plist ~/dotemacs/mac-preferences/.GlobalPreferences.plist
  # store an xml copy for easy inspection
  cp ~/dotemacs/mac-preferences/.GlobalPreferences.plist ~/dotemacs/mac-preferences/.GlobalPreferences.xml
  # WARNING: plutil is DESTRUCTIVE; it MODIFIES the file argument IN PLACE, that's why we're copying it first before converting it.
  plutil -convert xml1 ~/dotemacs/mac-preferences/.GlobalPreferences.xml
  # to convert back to .plist, use `plutil -convert binary1 .GlobalPreferences.plist`
}

backup-app-preferences(){
  # backup app preferences, mainly intended for custom App Shortcuts defined in System > Keyboard > App Shortcuts.
  # each .plist file contains app shortcuts (keybindings) for its corresponding app.
  # for example, app-preferences/Preferences/org.mozilla.firefox.plist contains custom keyboard shortcuts for Firefox.
  cp -R ~/Library/Preferences ~/dotemacs/app-preferences/
}


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
export _ZL_DATA="~/.zlua"
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

# change screenshot format to JPG
# defaults write com.apple.screencapture type jpg;killall SystemUIServer
alias screenshot-format-jpg="defaults write com.apple.screencapture type jpg;killall SystemUIServer"
# change screenshot format back to PNG
# defaults write com.apple.screencapture type png;killall SystemUIServer
alias screenshot-format-png="defaults write com.apple.screencapture type png;killall SystemUIServer"

# decrease notification display time
# defaults write com.apple.notificationcenterui bannerTime 4

# fix non-retina screen subpixel antialiasing on Mojave
# defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO
# enable "Use font smoothing when available" in Preferences > General

# fix Preview not remembering open files
defaults write com.apple.Preview NSQuitAlwaysKeepsWindows -bool true

# enable slow genie effect
defaults write com.apple.dock slow-motion-allowed -bool true

# ensure an appropriate bash version is installed (for associative arrays, etc) -- from https://clubmate.fi/upgrade-to-bash-4-in-mac-os-x/
# brew install bash
# Add the new shell to the list of allowed shells
# sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
# Change to the new shell
# chsh -s /usr/local/bin/bash

configure-gitdiff-colors(){
  # for gitdiff
  brew install diff-so-fancy
  git config --global core.pager "diff-so-fancy | less --tabs=4 -RFX"

  ## The default Git colors are not optimal. The colors used for the diff-so-fancy screenshot:
  git config --global color.diff-highlight.oldNormal    "red bold"
  git config --global color.diff-highlight.oldHighlight "red bold 52"
  git config --global color.diff-highlight.newNormal    "green bold"
  git config --global color.diff-highlight.newHighlight "green bold 22"

  git config --global color.diff.meta       "11"
  git config --global color.diff.frag       "magenta bold"
  git config --global color.diff.commit     "yellow bold"
  git config --global color.diff.old        "red bold"
  git config --global color.diff.new        "green bold"
  git config --global color.diff.whitespace "red reverse"
}

# for touchbar/command-line keyboard switcher:
# git clone https://github.com/myshov/xkbswitch-macosx
# ln -s /Users/luka/dotemacs/xkbswitch-macosx/bin/xkbswitch /usr/local/bin

# don't forget to apply Karabiner-Elements configuration (to be usable before login) - Karabiner Preferences > Misc Tab > System Default Configuration > * Copy the current configuration...
# in the same tab, disable Show icon in menu bar

export PATH=/usr/local/bin:~/.composer/vendor/bin:$PATH
#export PATH=/Applications/XAMPP/bin:$PATH

#PS1="\H:\W \u\$ "
PS1="\u: \W$ "
export PS1

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
#ssh-add ~/.ssh/multiple_id_rsa 2>/dev/null

ssh-add ~/Documents/luka/luka.ge/ssh/luka_ge_id_rsa 2>/dev/null
#ssh-add ~/Documents/ardi/ssh/ardi_id_rsa 2>/dev/null
#ssh-add ~/Documents/bookulus/ssh-key/bookulus.ge.id_rsa 2>/dev/null
#ssh-add ~/Documents/lb/ssh/crmfrontend_id_rsa 2>/dev/null

#ssh-add ~/Documents/alpha/ssh-key/alpha_id_rsa 2>/dev/null
ssh-add ~/Documents/alpha/ssh-key/live/alpha_live_id_rsa 2>/dev/null

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
ff(){
    clear
    f "$*"
}
# find running process by name
fp(){
  ps aux | grep "$*"
}

skip(){
  # -U doesn't work (supposed to skip .gitignore/ignore files)
  # ag respects .gitignore, but only from git root directory, so specifically exclude node_modules (throwing a lot of errors for deep paths)
  grep -v "$*"
}

# just output the nth column of tabular data. e.g. `ls -l | nth 3` only leaves the owner column.
nth(){
  awk "{ print \$$1 }"
}

line(){
  head -n $1 | tail -n 1
}

# has non-greedy matching
alias re="perl -pe"

# TODO. it will be an escaping hell so leave it to future me.
# rx(){
#   sed 's/'$1'/'$2'/g'
# }

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
	    # COMMENTED; don't auto-add everything
      # hg add . 2>/dev/null
        hg commit -m "$*" 2>/dev/null
    else
	    # COMMENTED; don't auto-add everything
      # git add . 2>/dev/null
        git commit -a -m "$*" 2>/dev/null
    fi
}
# quick commit (cam stands for git commit -a -m )
cam(){
    # git add . 2>/dev/null
    vcs-commit "$*"
}
vcs-checkout-master(){
    git checkout master
}
gcm(){
    vcs-checkout-master
}
ga.(){
  git add .
}
gr.(){
  git reset .
}
gc.(){
  read -p "Checkout ALL files in current directory? " -n 1 -r
  if [[ $REPLY =~ ^[Yy]$ ]]; then
     git checkout .
  fi
}
gcd(){
  if [ $(pwd) = "/Users/luka/Development/Flow/flow-services" ] \
  || [ $(pwd) = "/Users/luka/Development/Flow/flow-react-native" ] \
  || [ $(pwd) = "/Users/luka/Development/Flow/flow-crm" ] \
  || [ $(pwd) = "/Users/luka/Development/Flow/flow-crm/app" ];
    then
        git checkout develop
    else
        git checkout dev
   fi
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
        ssh -o 'PasswordAuthentication no' Luka.Ramishvili@crmfrontend-dev.lb.ge "cd /projects/lb && git pull"
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
    elif [ $(pwd) = "/projects/transit" ]
    then
      git push
      ssh root@carload.ge "cd /projects/carload && git pull"
    elif [ $(pwd) = "/projects/carrent" ]
    then
      git push
      ssh root@apps.luka.ge "cd /var/lib/jenkins/workspace/carrent && git pull"
    elif [ $(pwd) = "/projects/wvi" ] || [ $(pwd) = "/projects/wvi/Layout" ] || [ $(pwd) = "/projects/wvi/Solution" ]
    then
      git push
      ssh root@apps.luka.ge "cd /var/lib/jenkins/workspace/wvi && git pull"
    elif [ $(pwd) = "/projects/agro" ]
    then
      git push
      ssh root@apps.luka.ge "cd /var/lib/jenkins/workspace/agro && git pull"
    elif [ $(pwd) = "/projects/bloom" ]
    then
      git push
      ssh root@apps.luka.ge "cd /var/lib/jenkins/workspace/bloom-flora && git pull"
    elif [ $(pwd) = "/projects/blacktomato.ge" ]
    then
      git push
      ssh root@apps.luka.ge "cd /var/lib/jenkins/workspace/blacktomato && git pull"
    elif [ $(pwd) = "/projects/ardi/mobile" ]
    then
      git push
      ssh root@213.131.38.12 "cd /var/www/html/mobile && git pull"
    elif [ $(pwd) = "/projects/alpha/alpha-web" ]
    then
      git push
      ssh zerogravity@pay.alpha.ge "cd /projects/alpha && git pull"
    elif [ $(pwd) = "/projects/zero-gravity-website" ]
    then
      git push
      ssh root@apps.luka.ge "cd /usr/share/nginx/html/zerogravity.ge && git pull"
    elif [ $(pwd) = "/projects/amra" ] || [ $(pwd) = "/projects/amra/frontend" ]
    then
      git push
      ssh-add ~/Documents/alpha/ssh-key/staging/alpha_id_rsa
      ssh root@app.alpha.ge "cd /var/www/alpha.ge/public/amra/frontend && git pull && yarn install && yarn build --prod"
    elif [ $(pwd) = "/projects/promodesk" ]
    then
      git push
      ssh-add ~/Documents/promodesk/ssh-key/promodesk_id_rsa
      ssh rxsejbky@promodesk.ge "cd ~/promodesk && git pull"
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
}

# usage: md2docx source.md output.docx
md2docx(){
  if [ -f "$1" ];
  then
    pandoc -o "$2" -f markdown -t docx "$1"
    open "$2"
  else
    echo ".md file not found"
  fi
}

mp3(){
  if [ -f "$1" ];
  then
      # https://stackoverflow.com/a/32280085/324220
      mp3_cmd=( ffmpeg -i "$1" -f mp3 -acodec libmp3lame -ab 19200 -ar 44100 -b 320 "$1.mp3" )
      printf '%q ' "${mp3_cmd[@]}"
      "${mp3_cmd[@]}"
    # open "$2"
  else
    echo "source file \"$1\" not found"
  fi
}
flac(){
  if [ -f "$1" ];
  then
      # https://stackoverflow.com/a/32280085/324220
      flac_cmd=( ffmpeg -i "$1" -f flac -ab 19200 -ar 44100 "$1.flac" )
      printf '%q ' "${flac_cmd[@]}"
      "${flac_cmd[@]}"
    # open "$2"
  else
    echo "source file \"$1\" not found"
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

aws-login(){
  ## we assume you:
  ## * have done `aws configure`
  ## * added the original access/secret key ids to ~/.aws/credentials
  ## * saved your arn inside ~/.aws/iam-user-arn
  ## * git init && git committed the original access/secret key values in ~/.aws
  echo "Enter AWS profile name (default violetpay-api-dev:"
  read AWS_PROFILE_NAME
  if [ !AWS_PROFILE_NAME ]; then
    AWS_PROFILE_NAME=violetpay-api-dev
  fi
  echo "Enter OTP code:"
  read OTP
  # OTP=$(echo "$*" | tr -d '\n\r\t ')
  IAM_USER_ARN="`cat ~/.aws/iam-user-arn | tr -d '\n'`"
  $(cd ~/.aws/ && git checkout credentials)
  export AWS_SESSION=$(aws sts get-session-token --profile $AWS_PROFILE_NAME --serial-number $IAM_USER_ARN --duration-seconds 129600 --token-code $OTP)
  export AWS_ACCESS_KEY_ID=$(echo $AWS_SESSION | jq '.Credentials.AccessKeyId' | sed 's/"//g')
  export AWS_SECRET_ACCESS_KEY=$(echo $AWS_SESSION | jq '.Credentials.SecretAccessKey' | sed 's/"//g')
  export AWS_SESSION_TOKEN=$(echo $AWS_SESSION | jq '.Credentials.SessionToken' | sed 's/"//g')
  if grep "\[" -i ~/.aws/credentials | tr -d '\r\t ' | grep -v "\[default\]" | grep -E '.'; then 
    echo "only [default] aws profile is supported; couldn't find a reliable ini parser without thousands of dependencies. PR if you can."
    echo "AWS_SESSION=$AWS_SESSION"
    ( set -o posix ; set ) | grep "^AWS_" | grep -v "AWS_SESSION="
  else
    echo "[default]
AWS_ACCESS_KEY_ID = $AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY = $AWS_SECRET_ACCESS_KEY
AWS_SESSION_TOKEN = $AWS_SESSION_TOKEN" > ~/.aws/credentials
  fi
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


upload-file(){
    echo "user@host:"
    read $HOST
    rsync -v -e ssh "$*" $HOST:~
}
upload-dir(){
    echo "user:"
    read $USER
    echo "host:"
    read $HOST
    echo "remote directory:"
    read $DEST
    rsync -r -a -v -e "ssh -l $USER" --delete $HOST:$DEST "$*"
}

# syntax highlighting, e.g. grep ... | code-bash
alias code-bash="highlight --syntax bash -O ANSI"
alias code-javascript="highlight --syntax bash -O ANSI"
alias code-clojure="highlight --syntax clojure -O ANSI"

# separate file for aliases
if [ -f ~/dotemacs/.bash_aliases ]; then
    source ~/dotemacs/.bash_aliases
fi


