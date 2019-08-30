
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

alias dotemacs="cd ~/dotemacs"
alias de=dotemacs

#alias s="git status -s"
#alias st="git status -s"
#alias l="git log"
#alias gl="git log"
alias s="vcs-status"
alias st="vcs-status"
alias l="vcs-log"
alias gl="vcs-log"
alias gd="vcs-diff"
alias pl="vcs-pull"
alias pl-command="command pl"
alias pull="vcs-pull"
#alias wl="cd /projects/wom && tail -f -n0 storage/logs/* | grep '#0'"
alias gc="git commit"
alias gca="git commit --amend"
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
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias emacs-osx="/usr/local/bin/emacs"

alias security-on="sudo spctl --master-enable"
alias security-off="sudo spctl --master-disable"

alias vbox-start="sudo /Library/Application\ Support/VirtualBox/LaunchDaemons/VirtualBoxStartup.sh restart"

#alias php="/Applications/XAMPP/bin/php"
alias head="/usr/bin/head"
alias mysql="/Applications/XAMPP/bin/mysql"
alias mysqldump="/Applications/XAMPP/bin/mysqldump"

alias vu="vagrant up"
alias vh="vagrant halt"

# shortcuts to set npm registry and avoid remembering the command and links
alias npm-registry-default="npm set registry \"http://registry.npmjs.org\""
# alias npm-registry-luka.ge="npm set registry \"http://luka.ge:4873\""
alias npm-registry-luka.ge="npm set registry \"http://lb-npm.luka.ge\""

alias "gitdiff"="git diff --color | diff-so-fancy"

alias luka.ge="ssh root@luka.ge"
alias apps="ssh root@172.104.147.98" # apps.luka.ge
alias crm="ssh Luka.Ramishvili@crmfrontend-dev.lb.ge"

#alias sshpwnuser="ssh pawwwn.com@188.93.88.26 -p 2232"
alias sshpwnroot="ssh root@188.93.88.26 -p 2232"
#alias sshw3wom="ssh webing@womanizor.webintelligence.de -p 222"
#alias sshwomlive="ssh ec2-user@womanizor.com"
#alias sshwomlive="ssh root@womanizor.com"
#alias sshwomdev="ssh root@dev.womanizor.com"
#
alias sshbk="ssh root@bookulus.ge"

alias tinker="rlwrap /Applications/XAMPP/bin/php artisan tinker"

alias "git-status-publish"="git rev-list --count publish/master..master"


alias fix-spotlight='find . -type d -name "node_modules" -exec touch "{}/.metadata_never_index" \;'


### project-specific aliases

alias w="cd /projects"
alias w-command="command w"
alias p="cd /projects"

# ici tinker (reuse tinker alias for rlwrap)
#alias t="cd /projects/ici && tinker"
#alias w="cd /projects/wom"
# wom gulp
#alias wg="cd /projects/wom && gulp watch"
# wom serve, then open served url and bring dev server process to front
#alias ws="cd /projects/wom && php artisan serve --host 0.0.0.0 --port 8000 & (i\
#fconfig | grep 192 | perl -pe 's|^.*?(192\.\d+\.\d+\.\d+).*$|http://\1:8000|' |\
# xargs open) && fg"
# vtb cd to dir & serve
#alias v="cd /projects/vtb/Layout"
#alias vs="cd /projects/vtb/Layout && gulp serve"
# worldvide vision
#alias p="cd /projects/pawn"
#alias pw="cd /projects/pawn && gulp watch"
# alias c="cd /projects/calo"
# alias cs="cd /projects/calo && gulp serve"
# alias k="cd /www/kalo/web/app/themes/wi-theme"
# alias kr="cd /www/kalo/web"
# alias ks="cd /www/kalo/resources && gulp watch"
alias b="cd /projects/bookulus"
alias bs="cd /projects/bookulus && npm run dev"
alias bw="cd /www/bookulus.ge/web"
alias bl="cd /www/bookulus.ge/web && tail -f -n0 wp-content/debug.log"
#alias k="cd /projects/kt/Layout"
#alias ks="cd /projects/kt/Layout && gulp serve"
#alias a="cd /projects/asb/Layout"
#alias asb="cd /projects/asb/Layout"
#alias asbs="cd /projects/asb/Layout && gulp serve"
alias lb="cd /projects/lb"
alias lbs="cd /projects/lb && gulp serve"
#created a script with the same name in /usr/local/bin
#alias lb-emacs="open /Applications/Emacs.app --args --eval=\"(lb-mode)\""
alias lbang="cd /projects/angular-lb"
alias lbangs="cd /projects/angular-lb && ng serve --open"
alias lw="cd /projects/lw/Layout"
alias lws="cd /projects/lw/Layout && gulp serve"
#alias ici="cd /projects/ici"
#alias i="cd /projects/ici"
#alias iw="cd /projects/ici/ && npm run watch && open http://ici.devv"
#alias is="cd /projects/ici/ && npm run watch && open http://ici.devv"
alias ald="cd /projects/ald/layout"
alias alds="cd /projects/ald/layout && gulp serve"
alias bt="cd /projects/bt"
alias bts="cd /projects/bt && gulp serve"
#alias c="cd /projects/cx/Solution/HelixCore.WebApp"
#alias cx="cd /projects/cx/Solution/HelixCore.WebApp"
#alias cs="cd /projects/cx/Solution/HelixCore.WebApp && gulp webserver"
#alias p="cd /projects/pens"
#alias pn="cd /projects/pens"
#alias pens="cd /projects/pens"
#alias penserve="cd /projects/pens && npm run dev"


