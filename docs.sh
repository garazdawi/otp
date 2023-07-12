#!/bin/bash

set -xe

APPS=${1:-*}

shopt -s nullglob

function is_bin_in_path {
  builtin type -P "$1" &> /dev/null
}

if ! is_bin_in_path iex; then
    if [ ! -d elixir ]; then
        git clone -b main https://github.com/elixir-lang/elixir
        (cd elixir && make)
    fi
    export PATH=`pwd`/elixir/bin:$PATH
fi

if ! is_bin_in_path ex_doc; then
    if [ ! -d ex_doc ]; then
        git clone -b lukas/fix-erlang-doc-support https://github.com/garazdawi/ex_doc
    else
        (cd ex_doc && git pull origin lukas/fix-erlang-doc-support)
    fi
    (cd ex_doc && mix deps.get && mix escript.build)
    export PATH=`pwd`/ex_doc/:$PATH
fi


for app in $(ls -d lib/${APPS}/ebin | awk -F "/" '{print $2}'); do
    echo "Creating docs for $app"
    VSN=$(cat lib/$app/vsn.mk | grep -i "^${app}_vsn" | awk '{print $3}')
    erl -noinput -eval "docgen_xml_to_markdown:convert_application(${app}), halt()"
    ./insert_chunks.es lib/$app/ebin/*.beam
    APP="lib/$app" ex_doc $app "${VSN}" "lib/$app/ebin" -o "docs/$app" -c ex_doc.exs || exit
done

if [ "$APPS" = '*' ] || [ $APPS = 'erts' ]; then
    app=erts
    VSN=$(cat $app/vsn.mk | grep -i "^VSN" | awk '{print $3}')
    erl -noinput -eval "docgen_xml_to_markdown:convert_application(${app}), halt()"
    ./insert_chunks.es $app/preloaded/ebin/*.beam
    APP=$app ex_doc $app "${VSN}" "$app/preloaded/ebin" -o "docs/$app" -c ex_doc.exs || exit
fi
