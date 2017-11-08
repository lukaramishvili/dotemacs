
export PATH=/usr/local/bin:~/.composer/vendor/bin:$PATH
#export PATH=/Applications/XAMPP/bin:$PATH

#PS1="\H:\W \u\$ "
#export PS1

ssh-add ~/.ssh/multiple_id_rsa

source ~/wp-completion.bash

# we can divide these into modules; "source ./.bash/.bash_aliases" etc

#both emacs / emacs -nw causes inserting garbled text in the first open buffer
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias emacs-osx="/usr/local/bin/emacs"

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
alias sshwomcam="ssh root@cam.womanizor.com"

alias tinker="rlwrap /Applications/XAMPP/bin/php artisan tinker"

alias "git-status-publish"="git rev-list --count publish/master..master"

# quick commit (cam stands for git commit -a -m )
cam(){
    # make wrapping commit message in quotes unnecessary
    # (WARNING: doesn't work on (parens), only when escaped or, ironically, in quotes)
    # everything after $ qd ..., including spaces, will be passed
    # for reference: $@ would wrap each word (separated by space) in separate quotes
    # ..e.g.: if using $@, qd foo bar => cam "foo" "bar"
    git commit -a -m "$*"
}
# deploy - git push and update to server
d(){
    if [ $(pwd) = "/projects/wom" ]
    then
        # wom deploy
        git push publish && wget https://womanizor.com/deploy -O /dev/null
    else
        git push
        # TODO other projects' deploy paths
    fi
           
}
# quick deploy (commits with text after command, pushes to git and updates server code)
qd(){
    # show status for debugging, if something goes wrong
    git status
    # no parens without quotes - see comment about $* in cam()
    cam "$*"
    if [ $(pwd) = "/projects/wom" ]
    then
        git push publish
        d # deploy project
        git push # also push to alternate remote
    else
        git push
    fi
}

alias s="git status"
alias st="git status"
alias l="cd /projects/wom && tail -f -n0 storage/logs/* | grep '#0'"
alias c="git commit"
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
alias v="cd /projects/vtb/Layout"
alias vs="cd /projects/vtb/Layout && gulp serve"   
