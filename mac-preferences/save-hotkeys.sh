#!/bin/sh
# save-hotkeys.sh
# Tons of thanks to: https://newbedev.com/how-can-i-migrate-all-keyboard-shortcuts-from-one-mac-to-another

DESTFILE=~/dotemacs/mac-preferences/install-hotkeys.sh
echo '#!/bin/bash' > $DESTFILE

defaults find NSUserKeyEquivalents | sed -e "s/Found [0-9]* keys in domain '\\([^']*\\)':/defaults write \\1 NSUserKeyEquivalents '/" -e "s/    NSUserKeyEquivalents =     {//"  -e "s/};//" -e "s/}/}'/" >> $DESTFILE
echo killall cfprefsd >> $DESTFILE
chmod a+x $DESTFILE
