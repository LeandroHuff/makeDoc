#!/usr/bin/env bash

#MD#Markdown Document Generator
#MD
#MD**Description**: Markdown Document Generator.
#MD**File**       : genMarkDown.sh
#MD**Author**     : Leandro - leandrohuff@programmer.net
#MD**Date**       : 2025-09-16
#MD**Version**    : 1.0.0
#MD**Copyright**  : CC01 1.0 Universal
#MD**Details**    : Generate a formatted markdown file from a source code.
#MD                 Read source code line-by-line and save a documentation for tags.
#MD
#MD## Constants
#MD
#MD*vector integer* **Version** = *(1 0 0)*
declare -a -i -r Version=(1 0 0)
#MD
#MD## Variables
#MD
#MD*string* **Source** = *''*
declare Source=""
#MD*string* **Destine** = *''*
declare Destine=""
#MD
#MD## Functions
#MD
#MD*none* **logFail**( *string* **message** ) : *string*
#MDPrint a failure log message.
function logFail() { echo -e "\033[91mfailure:\033[0m $*" ; }
#MD
#MD*integer* **unsetVars**( **none** ) : *none*
#MDUnset and unload global variables.
function unsetVars()
{
    unset -v Source
    unset -v Destine
    return 0
}
#MD
#MD*integer* **_exit**( **none** ) : *none*
#MDEnd log, stop libShell, deinitialize variables and exit an error code.
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
#MD
#MD*integer* **_help**( **none** ) : *string*
#MDPrint an help message.
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
#MD
#MD*integer* **parseArgs**( *vector string* **$@** ) : *none*
#MDParse command line parameters.
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
#MDGet a tag name from parameter.
function getTag()   { echo -n "${1%"\#MD"*}"  ; }

#MDGet a value from parameter.
function getValue() { echo -n "${1/#*"\#MD"}" ; }

#MD
#MD*integer* **main**( *vector string* **$@** ) : *none*
#MDRun main application.
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
    echo > "${Destine}"
    if [ $? -ne 0 ] ; then
        logF "Could not create ( ${Destine} ) file."
        return 1
    fi
    while read -e line
    do
        [ -z "${line}" ] && continue
        if [[ "${line:0:3}" == "#MD" ]] ; then
            echo "${line:3}" >> "${Destine}"
        fi
    done < "${Source}"
    logS "Documentation generated."
    return 0
}

#MD###Script Entry Point
#MDCall *main()* function and pass all command line parameters.
#MDafter return from main, call _exit() function passing to it
#MDthe error code returned by main().
main "$@"
_exit $?
