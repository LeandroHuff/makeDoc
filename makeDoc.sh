#!/usr/bin/env bash

#/D # Make Documentation from Source Code
#/D
#/D |                 | Description                                  |
#/D |----------------:|:---------------------------------------------|
#/D | Brief           | Export Documentation from taged lines.       |
#/D | File            | makeDoc.sh                                   |
#/D | Author          | Leandro - leandrohuff@programmer.net         |
#/D | Date            | 2025-09-16                                   |
#/D | Version         | 1.0.0                                        |
#/D | Copyright       | CC01 1.0 Universal                           |
#/D
#/D **Details**
#/D Save formatted lines from source code into documentation file.
#/D Read source code line-by-line and save prefixed lines by tag '/D' to file.
#/D C/C++ source code lines start with tag '//D' and Shell Script lines start with tag '#/D'.
#/D Only those lines started with apropriate tags are sent to respective documentation files.
#/D Mixed documentation can co-exist at same source code, one for local documentation purpose and another to be exported to apropriate documentation file.
#/D All lines taged to export was documented using Markdown format, the document exported can be read by an Markdown program reader.
#/D Bellow is the documentation exported from source code by its own shell script program makeDoc.sh.
#/D
#/D ## Constants
#/D
#/D *integer*[] **Version** = (*1 0 0*)
declare -a -i -r Version=(1 0 0)
#/D
#/D ## Variables
#/D
#/D *string* **Source** = *''*
declare Source=""
#/D *string* **Destine** = *''*
declare Destine=""
#/D
#/D ## Functions
#/D
#/D ### logFail()
#/D
#/D **Function**: *none* **logFail**( *string* ) : *string*
#/D Print a failure log message.
#/D **Parameter**:
#/D *string*    String log message.
#/D **Result**:
#/D *string*    Formatted log message.
#/D **Return**:
#/D *none*
function logFail() { echo -e "\033[91mfailure:\033[0m $*" ; }
#/D
#/D ### unsetVars()
#/D
#/D **Function**: *integer* **unsetVars**( *none* ) : *none*
#/D Unset global variables.
#/D **Parameter**:
#/D *none*
#/D **Result**:
#/D *none*
#/D **Return**:
#/D *integer* 0 Success
function unsetVars()
{
    unset -v Source
    unset -v Destine
    return 0
}
#/D
#/D ### _exit()
#/D
#/D **Function**: *integer* **_exit**( *none* ) : *none*
#/D End log, stop libShell, deinitialize variables and exit an error code.
#/D **Parameter**:
#/D *none*
#/D **Result**:
#/D *none*
#/D **Return**:
#/D *integer* 0 Success
function _exit()
{
    local code=$([ -n "$1" ] && echo $1 || echo 0)
    logEnd
    libStop
    unsetVars
    exit $code
}
# Source libShell.sh
# Call libInit()
# Call libSetup()
# Call logBegin()
source libShell.sh || { logFail "Load libShell.sh"       ; _exit 1 ; }
libInit            || { logFail "Initialize libShell.sh" ; _exit 1 ; }
libSetup -d -l 1   || { logFail "Setup libShell.sh"      ; _exit 1 ; }
logBegin           || { logFail "Initialize Log."        ; _exit 1 ; }
#/D
#/D ### _help()
#/D
#/D **Function**: *integer* **_help**( *none* ) : *string*
#/D Print an help message.
#/D **Parameter**:
#/D *none*
#/D **Result**:
#/D *string*    Help message.
#/D **Return**:
#/D *integer* 0 Success
function _help()
{
    printf "Generate Markdown documentation from source files.
$(printLibVersion)
Script   Version: ${WHITE}$(genVersionStr ${Version[@]})${NC}
Syntax: $(getScriptName) [options]
Options:
-h | --help             Print help information about syntax and use.
-s | --source  <file>   Generate documentation from source file.
-d | --destine <file>   Generate documentation into destine file.
     --                 Send next parameters to libShell.
"
}
#/D
#/D ### parseArgs()
#/D
#/D **Function**: *integer* **parseArgs**( *string*[] ) : *none*
#/D Parse parameters from command line.
#/D **Parameter**:
#/D **-h** | **--help**            : Print help message on screen.
#/D **-s** | **--source**  <*file*>: Set a file as source of code and tags.
#/D **-d** | **--destine** <*file*>: Set a file as destine|target of markdown codes.
#/D      **--**              : Send next parameters to libShell.
#/D **Result**:
#/D *none*
#/D **Return**:
#/D *integer* 0 Success
#/D *integer* 1 Failure
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
            if isArgValue "$2"
            then
                shift
                Source="$1"
            else
                logF "Empty value for parameter -s | --source <file>"
                return 1
            fi
            ;;
        -d | --destine)
            if isArgValue "$2"
            then
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
#/D
#/D ### getTag()
#/D
#/D **Function**: *none* **getTag**( *string* ) : *string*
#/D Search and extract a tag from a string and print it back.
#/D **Parameter**:
#/D *string*    String to search for a tag name.
#/D **Result**:
#/D *string*    Tag name.
#/D **Return**:
#/D *none*
function getTag()   { echo -n "${1%"\#/D "*}"  ; }
#/D
#/D ### getValue()
#/D
#/D **Function**: *none* **getValue**( *string* ) : *string*
#/D Search and extract a value from a string and print it back.
#/D **Parameter**:
#/D *string*    String to search for a tag value.
#/D **Result**:
#/D *string*    Tag value.
#/D **Return**:
#/D *none*
function getValue() { echo -n "${1/#*"\#/D "}" ; }
#/D
#/D ### main()
#/D
#/D **Function**: *integer* **main**( *string*[] ) : *none*
#/D Main shell script application.
#/D **Parameter**:
#/D *string*[]  Parameter list from command line.
#/D **Result**:
#/D *none*
#/D **Return**:
#/D *integer* 0    Success
#/D *integer* 1..N Failure
function main()
{
    parseArgs "$@" || return $?
    if ! itExist "${Source}"
    then
        logF "Source file ( ${Source} ) not found."
        return 1
    fi
    logD "Source: ${Source}"
    if [ -z "${Destine}" ]
    then
        Destine="$(getName ${Source}).md"
    fi
    logD "Destine: ${Destine}"
    if itExist "${Destine}"
    then
        logW "Destine ( ${Destine} ) already exist."
        askToContinue 'Overwrite it'
        [ $? -eq 0 ] || return 1
    fi
    echo -n > "${Destine}"
    if [ $? -ne 0 ]
    then
        logF "Could not create ( ${Destine} ) file."
        return 1
    fi
    while read -e line
    do
        [ -n "${line}" ] || continue
        if [[ "${line:0:4}" == "#/D " ]] || \
           [[ "${line:0:4}" == "//D " ]]
        then
            logD "${line:4}"
            echo "${line:4}" >> "${Destine}"
        elif [[ "${line:0:3}" == "#/D" ]] || \
             [[ "${line:0:3}" == "//D" ]]
        then
            logD "${line:3}"
            echo "${line:3}" >> "${Destine}"
        else
            logD "${line}"
        fi
    done < "${Source}"
    logS "Documentation generated."
    return 0
}
#/D
#/D ### Shell Script Program
#/D
#/D Call **main**() function.
#/D Pass all command line parameters to main.
main "$@"
_exit $?
