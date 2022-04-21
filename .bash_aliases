
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
alias gdc="git diff --cached"
alias gcp="git checkout -p"
alias gap="git add -p"
grd(){
  if [ $(pwd) = "/Users/luka/Development/Flow/flow-services" ] \
  || [ $(pwd) = "/Users/luka/Development/Flow/flow-react-native" ] \
  || [ $(pwd) = "/Users/luka/Development/Flow/flow-crm" ] \
  || [ $(pwd) = "/Users/luka/Development/Flow/flow-crm/app" ];
    then
        git rebase develop
    else
        git rebase dev
   fi
}
alias grm="git rebase master"
alias gmm="git merge master"
alias gmd="git merge dev"
alias gr="git rebase"
alias grc="git rebase --continue"
alias gra="git rebase --abort"
alias gma="git merge --abort"


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
alias sshgsf="ssh-add /docs/gsf/ssh-key/gsf_id_rsa && ssh root@194.195.241.83"
alias sshamra="ssh-add /docs/amra/ssh-key/amra_id_rsa && ssh cld9-6997@195.69.143.183"
alias sshpromodesk="ssh-add ~/Documents/promodesk/ssh-key/promodesk_id_rsa && ssh rxsejbky@promodesk.ge"
alias sshtrustdev="ssh-add ~/Documents/trust/ssh-key/trust_id_rsa.pem && ssh ec2-user@3.70.14.199"
alias sshdigitalnews="ssh-add ~/Documents/digitalnews/ssh-key/digitalnews_id_rsa && ssh root@170.187.188.143"
alias sshbadagoni="ssh-add ~/Documents/badagoni/ssh-key/badagoni_id_rsa && ssh root@159.89.20.42"

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

alias rnra="npx react-native run-android"
alias rnri="npx react-native run-ios"
alias rnrid="npx react-native run-ios --device"
alias rnrisim="npx react-native run-ios --simulator=\"iPhone 11 Pro Max\""

alias yri="yarn run ios"
alias yrs="yarn run storybook"
alias yra="yarn run android"
alias ys="yarn start"
alias r="yarn start"

alias ns="npm start"

alias arm="ssh manjaro@manjaro.arm"
alias manjaro="ssh manjaro@manjaro.arm"

alias jhbuild="PATH=.new_local/bin:$PATH jhbuild"

alias noderepl="ts-node -O '{\"module\": \"commonjs\"}'"

alias ns="host -t ns"


alias syncthing-reset-folder="curl -X POST -H \"X-API-Key: $SYNCTHING_API_KEY\" http://localhost:8384/rest/system/reset?folder=$SYNCTHING_RESET_FOLDER_NAME"

alias syncthing-connect-port="(sleep 2 && web localhost:9596) & ssh -L 9596:localhost:8384 root@luka.ge"


#### project-specific aliases

#alias sshardi="mosh root@213.131.38.12"
alias sshardi="ssh root@mobile.ardi.ge"

alias sshalpha="mosh root@139.162.169.65" # IP of app.alpha.ge
#alias sshlivealpha="echo password: Alpha2021@; mosh zerogravity@185.69.172.54"
alias sshlivealpha="echo password: Alpha2021@; ssh zerogravity@185.69.172.54"

alias spnews-live="ssh-add ~/Documents/spnews/ssh-key/spnews_live_rsa && ssh ubuntu@135.125.238.78"

alias sshbk="ssh root@bookulus.ge"

alias mssqld-init="docker run -d --name sql_server_demo -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=PassWord2019123' -p 1433:1433 microsoft/mssql-server-linux"

alias mssqld="docker start 9a511a57ea7367246576a266cc295d942c0e3c58e08a514394d12cd93c19f5f7"

alias mssql-connect="mssql -u sa -p PassWord2019123"

alias sshradius="ssh root@104.248.82.163"
alias sshkalo="ssh-add ~/Documents/kalo/ssh-key/kalo_id_irsa && ssh root@kalo.ge -p 6781"
alias sshkalodev="echo JsH5MrxcLhC5ePU | pbcopy && ssh root@173.249.60.231"

alias fbdeploy-staging-admin="npm run build && firebase deploy --only hosting:envite-staging-admin"
alias fbdeploy-vendor-chat="npm run build && firebase deploy --only hosting:envite-vendor-chat"
alias fbdeploy-vendor-script="npm run build && firebase deploy --only hosting:envite-vendor-script"
alias fbdeploy-staging-chat="npm run build:staging && firebase deploy --only hosting:envite-staging-chat"
alias fbdeploy-staging-script="npm run build:staging && firebase deploy --only hosting:envite-staging-script"

alias filezilla-nossh="SSH_AUTH_SOCK=\"\"; /Applications/FileZilla.app/Contents/MacOS/filezilla"

alias sst-start="cd aws-stacks; sst start --stage pr179 --increase-timeout"
# to resume non-debug normal lambda functioning, we need to deploy sst
alias sst-end="cd aws-stacks; sst deploy --stage pr179"

#### END project-specific aliases
