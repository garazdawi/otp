#!/bin/bash

set -x

APPS=${1:-*}

shopt -s nullglob

for app in $(ls -d lib/${APPS}/ebin | awk -F "/" '{print $2}'); do
    echo "Creating docs for $app"
    VSN=$(cat lib/$app/vsn.mk | grep -i "^${app}_vsn" | awk '{print $3}')
    APP="lib/$app" $HOME/git/ex_doc/ex_doc $app "${VSN}" "lib/$app/ebin" -o "docs/$app" -c ex_doc.exs || exit
done

if [ $APPS = '*' ] || [ $APPS = 'erts' ]; then
    app=erts
    VSN=$(cat $app/vsn.mk | grep -i "^VSN" | awk '{print $3}')
    APP=$app $HOME/git/ex_doc/ex_doc $app "${VSN}" "lib/$app/ebin" -o "docs/$app" -c ex_doc.exs || exit
fi
