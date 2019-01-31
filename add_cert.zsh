#!/bin/zsh
#
# from https://gist.github.com/Artistan/5219484efb2fe51cd064175b3d0d5971
#
if [ -z "$1" ]; then
	echo "provide a domain as an argument"
	exit;
fi

d=`date +%Y-%m-%d`
p=~/$1$d.pem
f=~/$1$d.cer

touch $f
touch $p

#  path added -- brew openssl....
#  echo 'export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"' >> ~/.zshrc

# get pem file
openssl s_client -showcerts -connect "$1:443" -servername $1  </dev/null | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' > $p
# https://stackoverflow.com/questions/13732826/convert-pem-to-crt-and-key
openssl x509 -inform PEM -in $p -outform DER -out $f

#cat $f;exit;
   # https://github.com/laravel/homestead/pull/773
   # https://stackoverflow.com/questions/45263265/use-ssl-on-laravel-homestead
   # https://deliciousbrains.com/ssl-certificate-authority-for-local-https-development/
   echo "adding cert $f  to trusted root certs"
   echo "Don't forget to open Keychain Access and set the certificate to Always Trust"
   if [[ $( sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain $f ) ]]
   then
        echo "killing chrome to get the new certificate"
        #pkill -a -i "Google Chrome"
   fi
