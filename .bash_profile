
export PATH=/usr/local/bin:~/.composer/vendor/bin:$PATH
#export PATH=/Applications/XAMPP/bin:$PATH

#PS1="\H:\W \u\$ "
#export PS1

ssh-add ~/.ssh/multiple_id_rsa

ssh-add ~/Documents/luka/luka.ge/ssh/luka_ge_id_rsa

source ~/wp-completion.bash

# we can divide these into modules; "source ./.bash/.bash_aliases" etc

#both emacs / emacs -nw causes inserting garbled text in the first open buffer
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias emacs-osx="/usr/local/bin/emacs"

f(){
    # -U doesn't work (supposed to skip .gitignore/ignore files)
    # ag respects .gitignore, but only from git root directory, so specifically exclude node_modules (throwing a lot of errors for deep paths)
    ag -R --ignore node_modules "$*" .
}

alias php="/Applications/XAMPP/bin/php"
alias head="/usr/bin/head"
alias mysql="/Applications/XAMPP/bin/mysql"
alias mysqldump="/Applications/XAMPP/bin/mysqldump"

alias "gitdiff"="git diff --color | diff-so-fancy"

alias sshpwnuser="ssh pawwwn.com@188.93.88.26 -p 2232"
alias sshpwnroot="ssh root@188.93.88.26 -p 2232"
#alias sshw3wom="ssh webing@womanizor.webintelligence.de -p 222"
#alias sshwomlive="ssh ec2-user@womanizor.com"
alias sshwomlive="ssh root@womanizor.com"
alias sshwomdev="ssh root@dev.womanizor.com"

alias tinker="rlwrap /Applications/XAMPP/bin/php artisan tinker"

alias "git-status-publish"="git rev-list --count publish/master..master"

# quick commit (cam stands for git commit -a -m )
cam(){
    # make wrapping commit message in quotes unnecessary
    # (WARNING: doesn't work on (parens), only when escaped or, ironically, in quotes)
    # everything after $ qd ..., including spaces, will be passed
    # for reference: $@ would wrap each word (separated by space) in separate quotes
    # ..e.g.: if using $@, qd foo bar => cam "foo" "bar"
    git add .
    git commit -a -m "$*"
}
# deploy - git push and update to server
d(){
    if [ $(pwd) = "/projects/wom" ]
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
    else
        git push
        # TODO other projects' deploy paths
    fi
           
}
# quick deploy (commits with text after command, pushes to git and updates server code)
qd(){
    # show status for info or debugging, if something goes wrong
    git status
    # no parens without quotes - see comment about $* in cam()
    cam "$*"
    if [ $(pwd) = "/projects/wom" ]
    then
        d # deploy project
        git push # also push to alternate remote
    else
        d # deploy - will update server code on specific projects; git push otherwise
    fi
}

alias s="git status"
alias st="git status"
#alias l="cd /projects/wom && tail -f -n0 storage/logs/* | grep '#0'"
alias gc="git commit"
alias gw="gulp watch"
alias gwp="gulp watch --production"
alias gs="gulp serve"

# wom tinker (reuse tinker alias for rlwrap)
alias t="cd /projects/wom && tinker"
alias w="cd /projects/wom"
# wom gulp
alias wg="cd /projects/wom && gulp watch"
# wom serve, then open served url and bring dev server process to front
alias ws="cd /projects/wom && php artisan serve --host 0.0.0.0 --port 8000 & (i\
fconfig | grep 192 | perl -pe 's|^.*?(192\.\d+\.\d+\.\d+).*$|http://\1:8000|' |\
 xargs open) && fg"
# vtb cd to dir & serve
#alias v="cd /projects/vtb/Layout"
#alias vs="cd /projects/vtb/Layout && gulp serve"
# worldvide vision
alias v="cd /projects/don/Layout"
alias vs="cd /projects/don/Layout && gulp serve"
alias c="cd /projects/calo"
alias cs="cd /projects/calo && gulp serve"
alias k="cd /www/kalo/web/app/themes/wi-theme"
alias kr="cd /www/kalo/web"
alias ks="cd /www/kalo/resources && gulp watch"
alias b="cd /projects/bookulus"
alias bs="cd /projects/bookulus && npm run dev"
alias lb="cd /projects/lb"
alias lbs="cd /projects/lb && gulp serve"
