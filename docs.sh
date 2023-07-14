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
    if [ -f lib/$app/doc/src/ex_doc.exs ]; then
        EX_DOCS_EXS=lib/$app/doc/src/ex_doc.exs
    else
        EX_DOCS_EXS=$ERL_TOP/ex_doc.exs
    fi
    APP="lib/$app" ex_doc $app "${VSN}" "lib/$app/ebin" -o "docs/$app" -c $EX_DOCS_EXS || exit
done

if [ "$APPS" = '*' ] || [ $APPS = 'erts' ]; then
    app=erts
    VSN=$(cat $app/vsn.mk | grep -i "^VSN" | awk '{print $3}')
    erl -noinput -eval "docgen_xml_to_markdown:convert_application(${app}), halt()"
    ./insert_chunks.es $app/preloaded/ebin/*.beam
    if [ -f $app/doc/src/ex_doc.exs ]; then
        EX_DOCS_EXS=$app/doc/src/ex_doc.exs
    else
        EX_DOCS_EXS=$ERL_TOP/ex_doc.exs
    fi
    APP=$app ex_doc $app "${VSN}" "$app/preloaded/ebin" -o "docs/$app" -c $EX_DOCS_EXS || exit
fi

function system_guide_title {
    case $1 in
        installation_guide) echo -n "Installation Guide";;
        system_principles) echo -n "System Principles";;
        embedded) echo -n "Embedded Systems User's Guide";;
        getting_started) echo -n "Getting Started With Erlang";;
        reference_manual) echo -n "Erlang Reference Manual";;
        programming_examples) echo -n "Programming Examples";;
        efficiency_guide) echo -n "Efficiency Guide";;
        tutorial) echo -n "Interoperability Tutorial";;
        design_principles) echo -n "OTP Design Principles";;
        oam) echo -n "OAM Principles";;
        *) echo -n "$1";;
    esac
    return 0
}

if [ "$APPS" = '*' ] || [ $APPS = 'system' ]; then
    VSN=$(cat OTP_VERSION)
    for guide in installation_guide system_principles embedded getting_started reference_manual programming_examples efficiency_guide tutorial design_principles oam; do
        TITLE=$(system_guide_title $guide)
        echo "# $TITLE" > system/doc/$guide/1_$guide-readme.md
    done
    erl -noinput -eval "docgen_xml_to_markdown:convert_application(system), halt()"
    APP=$app ex_doc "Erlang System" "${VSN}" "lib/erl_interface/ebin" -o "docs/system" -c system/doc/ex_doc.exs || exit
fi


if [ "$APPS" = '*' ] || [ $APPS = 'index' ]; then
    function create_redirect {
        local DIR=$1
        shift
        rm -rf $DIR
        mkdir $DIR
        for app in $*; do
            local TITLE=$(system_guide_title $app)
            case $app in
                installation_guide |\
                system_principles |\
                embedded |\
                getting_started |\
                reference_manual |\
                programming_examples |\
                efficiency_guide |\
                tutorial |\
                design_principles |\
                oam)
                    LOCATION="system/1_$app-readme.html";;
                *) LOCATION="$app/index.html";;
            esac
            echo -e "# $TITLE\n\n<script>window.location.replace(\"$LOCATION\");</script>" > $DIR/$app.md
        done
    }
    VSN=$(cat OTP_VERSION)
    create_redirect system/doc/top/basic compiler erts kernel sasl stdlib
    create_redirect system/doc/top/database mnesia odbc
    create_redirect system/doc/top/oam os_mon snmp
    create_redirect system/doc/top/interfaces asn1 crypto diameter eldap erl_interface ftp inets jinterface megaco public_key ssh ssl tftp wx xmerl
    create_redirect system/doc/top/tools debugger dialyzer et observer parsetools reltool runtime_tools syntax_tools tools
    create_redirect system/doc/top/test common_test eunit
    create_redirect system/doc/top/docs erl_docgen edoc
    create_redirect system/doc/top/system installation_guide system_principles embedded getting_started reference_manual programming_examples efficiency_guide tutorial design_principles oam
    APP=$app ex_doc "Erlang/OTP" "${VSN}" "lib/erl_interface/ebin" -o "docs/index" -c system/doc/top/ex_doc.exs || exit
    cp -r docs/index/* docs/
    rm -rf docs/index
fi
