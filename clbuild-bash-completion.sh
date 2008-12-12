#!/bin/bash

# Dublicated from the main clbuild script
clbuild_dir=$(pwd)

project_files="projects wnpp-projects my-projects implementations"

applications="clim-launcher listener gsharp climacs closure beirc climplayer
demodemo clim-alerts eclipse hunchentoot webdav parse-xml validate-xml
valideate-relax-ng html-to-xhtml xhtml-to-html xuriella vecto-demo
adw-charting-demo ltk-demo clpython"

clbuild_commands="help update install uninstall diff slime lisp preloaded check\
                  list recompile run"

global_options=" --help --implementation --long-help"

cmd=""
function _clbuild_set_cmd {
    # somewhat at-hoc: breaks when strange options are used, but does the job
    # otherwise
    for arg in "$@"; do
        if echo "$clbuild_commands" | grep -- "\b$arg\b" > /dev/null; then
            cmd=$arg
        fi
    done
}


function _clbuild_projects {
    local cur prev
    cur=${1}
    prev=${2}
    #opts="update install"
    projects=$(cut -d' ' -f1 $(for proj in ${project_files}; do echo ${clbuild_dir}/${proj}; done) | grep -ve '^#' | grep -v '^$')
    project_groups="--all-projects --main-projects --wnpp-projects --installed"
    COMPREPLY=( $(compgen -W "${project_groups} ${projects}" -- ${cur}) )
    return 0
}

function _clbuild_commands {
    local cur prev
    cur=${1}
    COMPREPLY=( $(compgen -W "${clbuild_commands}" -- ${cur}) )
    return 0
}

function _clbuild_implementations {
    local cur prev implementations
    cur=$1
    implementations=$(cut -d' ' -f1 ${clbuild_dir}/implementations | grep -v '^#' | grep -v '^$')
    COMPREPLY=( $(compgen -W "${implementations}" -- ${cur}) )
    return 0
}

function _clbuild_applications {
    local cur
    cur=$1
    COMPREPLY=( $(compgen -W "${applications}" -- ${cur}) )
    return 0
}

function _clbuild_completion {
    local cur prev implementations
    COMPREPLY=()
    #cur="${COMP_WORDS[COMP_CWORD]}"
    #prev="${COMP_WORDS[COMP_CWORD-1]}"
    # Ignore COMP_WORDS.  We get all we need passed as parameters
    clbuild=$1
    cur=$2
    prev=$3
    # set the `cmd' variable.
    _clbuild_set_cmd "$@"
    case $prev in
        --implementation)
            _clbuild_implementations $cur
            ;;
        update|install|uninstall)
            _clbuild_projects $cur
            ;;
        run)
            _clbuild_applications $cur
            ;;
        *clbuild)
            COMPREPLY=( $(compgen -W "${global_options} ${clbuild_commands}" -- ${cur}) )
            ;;
        *)
            if [ -n $cmd ]; then
                _clbuild_commands $cur
            fi
            ;;
    esac
    return 0
}
        
complete -F _clbuild_completion clbuild
