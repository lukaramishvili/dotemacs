
alias ~="cd ~"
alias ..="cd .."
## get rid of command not found ##
alias cd..='cd ..'
## a quick way to get out of current directory ##
alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'
alias .....='cd ../../../../'
alias .4='cd ../../../../'
alias .5='cd ../../../../..'

alias c="clear"
alias h='history'
alias j='jobs -l'

alias grep="grep --color=auto"

alias o="open"
alias opn="open"

# Start calculator with math support
alias bc='bc -l'

alias path='echo -e ${PATH//:/\\n}'
alias now='date +"%T"'
alias nowtime=now
alias nowdate='date +"%d-%m-%Y"'

# allow using aliases with xargs, e.g. `cat ... | xargs unescape` (unescape is an alias)
# https://stackoverflow.com/a/59842439/324220
alias xargs='xargs ' # create an xargs alias with trailing space.

alias unescape="printf '%b\n'"

alias dotemacs="cd ~/dotemacs"
alias de=dotemacs

alias bincat="hexdump -C"

#alias s="git status -s"
#alias st="git status -s"
#alias l="git log"
#alias gl="git log"
alias s="vcs-status"
alias st="vcs-status"
# long version
alias ss="git status"
alias sb="git status | grep 'On branch' | sed 's/^On branch //'"
alias sbc="git status | grep 'On branch' | sed 's/^On branch //' | pbcopy"
alias l="vcs-log"
alias gl="vcs-log"
alias gd="vcs-diff"
alias pl="vcs-pull"
alias pl-command="command pl"
alias pull="vcs-pull"
#alias wl="cd /projects/wom && tail -f -n0 storage/logs/* | grep '#0'"
alias gc="git commit"
alias gca="git commit --amend"
alias gpfl="git push --force-with-lease"

alias gw="gulp watch"
alias gwp="gulp watch --production"
alias gs="gulp serve"

# alias ping='ping -c 25 -s.2'

alias ifconfig="ifconfig en0"
alias ifconfig-all="command ifconfig"

# Resume wget by default
alias wget='wget -c'

alias open-ports='lsof -Pn -i4 -i6 | grep LISTEN'
alias ports=open-ports

# https://stackoverflow.com/a/41794118
# some features (e.g. --block-size) requires GNU coreutils (see .bashrc)
alias ll="ls -alF"
# see file sizes in kb/mb
alias lk="ls -laFS --block-size=K"
alias lkr="ls -laFSr --block-size=K"
#alias lm="ls -laFS --block-size=M"
#alias lmr="ls -laFSr --block-size=M"
# show and sort by file sizes (biggest at the top)
alias lz="ls -lhaFS"
# show and sort by file sizes (biggest at the bottom)
alias lzr="ls -lhaFSr"
# show sizes and sort by modified date (more recently modified files first)
alias lm="ls -hlFt"
# same (by modified date) but more recently modified at the bottom
alias lmr="ls -hlFtr"
# show sizes and sort by accessed date (more recently opened files first)
alias la="ls -hlFtu"
# same (by access date) but more recently opened at the bottom
alias lar="ls -hlFtur"
# I don't really appreaciate the default ls layout. I prefer more info and tabular, easily manipulated list format.
# -h is for human readable output, which doesn't round file sizes like --block-size does.
alias lscustom="ls -lhaF"
# separated alias to easily switch between default ls modes
alias ls="lscustom"

#both emacs / emacs -nw causes inserting garbled text in the first open buffer
# for speed in terminal, -q skips init file, default.el and package-initialize
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw -q"
alias emacs-osx="/usr/local/bin/emacs"

alias security-on="sudo spctl --master-enable"
alias security-off="sudo spctl --master-disable"

alias vbox-start="sudo /Library/Application\ Support/VirtualBox/LaunchDaemons/VirtualBoxStartup.sh restart"

#alias php="/Applications/XAMPP/bin/php"
#alias head="/usr/bin/head"
#alias mysql="/Applications/XAMPP/bin/mysql"
#alias mysqldump="/Applications/XAMPP/bin/mysqldump"

alias vu="vagrant up"
alias vh="vagrant halt"

# shortcuts to set npm registry and avoid remembering the command and links
alias npm-registry-default="npm set registry \"http://registry.npmjs.org\""
# alias npm-registry-luka.ge="npm set registry \"http://luka.ge:4873\""
alias npm-registry-luka.ge="npm set registry \"http://lb-npm.luka.ge\""

alias "gitdiff"="git diff --color | diff-so-fancy"
alias "gbl"="git branch --list"
alias "bl"="git branch --list"

alias luka.ge="ssh root@luka.ge"
alias apps="ssh root@172.104.147.98" # apps.luka.ge
# alias crm="ssh Luka.Ramishvili@crmfrontend-dev.lb.ge"
alias crm="cd /projects/flow/flow-crm"

#
alias sshbk="ssh root@bookulus.ge"

alias tinker="php artisan tinker"
alias t="php artisan tinker"
alias padb="php artisan backup:database"

alias "git-status-publish"="git rev-list --count publish/master..master"


alias fix-spotlight='find . -type d -name "node_modules" -exec touch "{}/.metadata_never_index" \;'


### project-specific aliases

alias w="cd /projects"
alias w-command="command w"
alias p="cd /projects"

# wom serve, then open served url and bring dev server process to front
#alias ws="cd /projects/wom && php artisan serve --host 0.0.0.0 --port 8000 & (i\
#fconfig | grep 192 | perl -pe 's|^.*?(192\.\d+\.\d+\.\d+).*$|http://\1:8000|' |\
# xargs open) && fg"
#alias b="cd /projects/bookulus"
#alias bs="cd /projects/bookulus && npm run dev"
#alias bw="cd /www/bookulus.ge/web"
#alias bl="cd /www/bookulus.ge/web && tail -f -n0 wp-content/debug.log"
alias lb="cd /projects/lb"
alias lbs="cd /projects/lb && gulp serve"
#created a script with the same name in /usr/local/bin
#alias lb-emacs="open /Applications/Emacs.app --args --eval=\"(lb-mode)\""
alias lbang="cd /projects/angular-lb"
alias lbangs="cd /projects/angular-lb && ng serve --open"

alias cr="cd /projects/carrent"
#alias tt="cd /projects/transit"
#alias bl="cd /projects/bloom"

alias bt="cd /projects/blacktomato.ge"

alias pa="php artisan"
alias pas="php artisan serve"
#alias t="php artisan tinker"
alias pat="php artisan tinker"

alias clojure="rlwrap clojure"
alias lf="lein figwheel"

alias rnra="react-native run-android"
alias rnri="react-native run-ios"
alias rnrid="react-native run-ios --device"
alias rnrisim="react-native run-ios --simulator=\"iPhone 11 Pro Max\""

alias yri="yarn run ios"
alias yrs="yarn run storybook"
alias yra="yarn run android"

alias ns="npm start"
