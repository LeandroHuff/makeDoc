#!/usr/bin/env bash

#§#Markdown Document Generator
#§
#§**Description**: Markdown Document Generator.
#§**File**       : genMarkDown.sh
#§**Author**     : Leandro - leandrohuff@programmer.net
#§**Date**       : 2025-09-16
#§**Version**    : 1.0.0
#§**Copyright**  : CC01 1.0 Universal
#§**Details**    : Generate a formatted markdown file from a source code.
#§                 Read source code line-by-line and save a documentation for tags.
#§
#§## Constants
#§
#§*vector integer* **Version** = *(1 0 0)*
declare -a -i -r Version=(1 0 0)
#§
#§## Variables
#§
#§*string* **Source** = *''*
declare Source=""
#§*string* **Destine** = *''*
declare Destine=""
#§
#§## Functions
#§
#§*none* **logFail**( *string* **message** ) : *string*
#§Print a failure log message.
function logFail() { echo -e "\033[91mfailure:\033[0m $*" ; }
#§
#§*integer* **unsetVars**( **none** ) : *none*
#§Unset and unload global variables.
function unsetVars()
{
    unset -v Source
    unset -v Destine
    return 0
}
#§
#§*integer* **_exit**( **none** ) : *none*
#§End log, stop libShell, deinitialize variables and exit an error code.
function _exit()
{
    local code=$([ -n "$1" ] && echo $1 || echo 0)
    logEnd
    libStop
    unsetVars
    exit $code
}

source libShell.sh || { logFail "Load libShell.sh"       ; _exit 1 ; }
libInit            || { logFail "Initialize libShell.sh" ; _exit 1 ; }
libSetup -d -l 1   || { logFail "Setup libShell.sh"      ; _exit 1 ; }
logBegin           || { logFail "Initialize Log."        ; _exit 1 ; }
#§
#§*integer* **_help**( **none** ) : *string*
#§Print an help message.
function _help()
{
printf "Generate Markdown documentation from source files.
$(printLibVersion)
Script   Version: ${WHITE}$(genVersionStr ${Version[@]})${NC}
Syntax: $(getScriptName) [options]
Options:
-h | --help             Print help information about syntax and use.
-s | --source <file>    Generate documentation for source file.
"
}
#§
#§*integer* **parseArgs**( *vector string* **$@** ) : *none*
#§Parse command line parameters.
function parseArgs()
{
    while [ $# -gt 0 ]
    do
        case "$1" in
        -h | --help)
            _help
            _exit 0
            ;;
        -s | --source)
            if isArgValue "$2" ; then
                shift
                Source="$1"
            else
                logF "Empty value for parameter -s | --source <file>"
                return 1
            fi
            ;;
        -d | --destine)
            if isArgValue "$2" ; then
                shift
                Destine="$1"
            else
                logF "Empty value for parameter -d | --destine <file>"
                return 1
            fi
            ;;
        --)
            shift
            libSetup "$@"
            break
            ;;
        *)
            logF "Unknown parameter or value ($1)."
            return 1
            ;;
        esac
        shift
    done
    return 0
}
#§Get a tag name from parameter.
function getTag()   { echo -n "${1%§*}"  ; }

#§Get a value from parameter.
function getValue() { echo -n "${1##*§}" ; }

#§
#§*integer* **main**( *vector string* **$@** ) : *none*
#§Run main application.
function main()
{
    parseArgs "$@" || return $?
    if ! itExist "${Source}" ; then
        logF "Source file ( ${Source} ) not found."
        return 1
    fi
    logD "Source: ${Source}"
    if [ -z "${Destine}" ] ; then
        Destine="$(getName ${Source}).md"
    fi
    logD "Destine: ${Destine}"
    if itExist "${Destine}" ; then
        logW "Destine ( ${Destine} ) already exist."
        askToContinue 10 'Overwrite it'
        [ $? -eq 0 ] || return 1
    fi
    echo § "${Destine}"
    if [ $? -ne 0 ] ; then
        logF "Could not create ( ${Destine} ) file."
        return 1
    fi
    local value
    while read -e line
    do
        logT "line: ${line}"
        [ -z "${line}" ] && continue
        value=$(getValue "${line}")
        logD "value: ${value}"
        if [ -n "${value}" ] ; then
            echo "${value}" >> "${Destine}"
            logI "${value}"
        fi
    done < "${Source}"
    logS "Documentation generated."
    return 0
}

#§###Script Entry Point
#§Call *main()* function and pass all command line parameters.
#§after return from main, call _exit() function passing to it
#§the error code returned by main().
main "$@"
_exit $?
