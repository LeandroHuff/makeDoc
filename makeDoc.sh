#!/usr/bin/env bash
##D # makeDoc
##D
##D | Type/Name | Description                           |
##D |----------:|:--------------------------------------|
##D |           | Export Documentation from taged lines |
##D | File      | makeDoc.sh                            |
##D | Author    | Leandro - leandrohuff@programmer.net  |
##D | Date      | 2025-09-16                            |
##D | Version   | 1.0.0                                 |
##D | Copyright | CC01 1.0 Universal                    |
##D
##D ---
##D ## Details
##D
##D Save formatted lines from source code into documentation file.
##D Read source code line-by-line and save prefixed lines by tag ??D to file.
##D C/C++ source code lines start with tag //D and Bash lines start with tag ##D.
##D Only those lines started by tags are exportedd to documentation files.
##D Mixed commented lines can co-exist at same source code, one for local documentation purpose and another to be exported to apropriate documentation file.
##D All lines are documented using Markdown format, the exported document can be read by an Markdown program reader.
##D
##D ---
##D ## Documentation Format
##D
##D ```
##D ??D # fileName
##D ??D
##D ??D | Type/Name | Description              |
##D ??D |----------:|:-------------------------|
##D ??D |           | Source Code Description  |
##D ??D | File      | fileName.sh              |
##D ??D | Author    | Name - author@e-Mail     |
##D ??D | Date      | YYYY-MM-DD               |
##D ??D | Version   | Version.Release.Features |
##D ??D | Copyright | Copyright description    |
##D ??D
##D ??D ---
##D ??D ## Index
##D ??D
##D ??D [Constants](#Constants)
##D ??D [Variables](#Variables)
##D ??D [Functions](#Functions)
##D ??D
##D ??D ---
##D ??D <a id="Constants"></a>
##D ??D ## Constants
##D ??D
##D ??D *type*[] **Name** = *value*
##D ??D
##D ??D [Index](#Index)
##D ??D
##D ??D ---
##D ??D <a id="Variables"></a>
##D ??D ## Variables
##D ??D
##D ??D *type* **Name** = *value*
##D ??D
##D ??D [Index](#Index)
##D ??D
##D ??D ---
##D ??D <a id="Functions"></a>
##D ??D ## Functions
##D ??D
##D ??D | Function Index              | Brief Description       |
##D ??D |----------------------------:|:------------------------|
##D ??D | [FunctionOne](#FunctionOne) | FunctionOne description |
##D ??D | [FunctionTwo](#FunctionTwo) | FunctionTwo description |
##D ??D | ...                         |                         |
##D ??D | [FunctionN](#FunctionN)     | FunctionN description   |
##D ??D
##D ??D <a id="FunctionOne"></a>
##D ??D ### FunctionOne
##D ??D
##D ??D *typeReturn* **FunctionOne**( *type* **parameter** ) : *typeResult*
##D ??D FunctionOne description.
##D ??D
##D ??D **Parameter**:
##D ??D *type* **parameter** : Parameter description.
##D ??D
##D ??D **Result**:
##D ??D *typeResult* : Result description.
##D ??D
##D ??D **Return**:
##D ??D *typeReturn* **value** : Return description
##D ??D
##D ??D [Functions](#Functions) | [Index](#Index)
##D ```
##D
##D ?? Change to:
##D
##D | Doc Tag   | Description                                                                |
##D |----------:|:---------------------------------------------------------------------------|
##D | ##D       | Bash, Zsh, Python, Perl, Ruby                                              |
##D | //D       | C/C++, C#, Java, JavaScript, Pascal/Object Pascal, Go, Swift, Kotlin, Rust |
##D | --D       | SQL, Ada, Haskell                                                          |
##D | ''D       | Visual Basic, VBScript                                                     |
##D | %%D       | LaTex, MATLAB                                                              |
##D
##D **Markdown Tags**:
##D
##D | MD Tag        | Description                                 |
##D |--------------:|:--------------------------------------------|
##D | \#            | Title, File Name                            |
##D | \#\#          | Title, Constants/Variables/Functions blocks |
##D | \#\#\#        | Title, Function Name                        |
##D | \* \*         | *Italic*, Types                             |
##D | \*\* \*\*     | **Bold**, Function and Varible Names        |
##D | \'\'\' \'\'\' | Code Blocks                                 |
##D | \---          | Horizontal Line                             |
##D
##D ---
##D ## Documentation Result
##D
##D <a id="logFail"></a>
##D ### functionName
##D
##D *type return* **functionName**( *type* **param 1** , ... , *type* **param N** ) : *type result*
##D Function description.
##D
##D **Parameter**:
##D *type* : Parameter description.
##D
##D **Result**:
##D *type* : Result description.
##D
##D **Return**:
##D *type* : Return description.
##D
##D [Function](#Functions) | [Index](#Index)
##D
##D ---
##D <a id="Index"></a>
##D ## Index
##D
##D [Constants](#Constants)
##D [Variables](#Variables)
##D [Functions](#Functions)
##D
##D ---
##D <a id="Constants"></a>
##D ## Constants
##D
##D *integer*[] **Version** = (*1 0 0*)
declare -a -i -r Version=(1 0 0)
##D
##D [Index](#Index)
##D
##D ---
##D <a id="Variables"></a>
##D ## Variables
##D
##D *string* **Source** = *''*
declare Source=""
##D *string* **Destine** = *''*
declare Destine=""
##D
##D [Index](#Index)
##D
##D ---
##D <a id="Functions"></a>
##D ## Functions
##D
##D | Name                      | Description                                              |
##D |--------------------------:|:---------------------------------------------------------|
##D | [logFail](#logFail)       | Print a failure log message.                             |
##D | [unsetVars](#unsetVars)   | Unset global variables.                                  |
##D | [_exit](#_exit)           | End log, stop libShell, deinitialize variables and exit. |
##D | [_help](#_help)           | Print an help message.                                   |
##D | [parseArgs](#parseArgs)   | Parse parameters from command line.                      |
##D | [main](#main)             | Main shell script application.                           |
##D
##D [Index](#Index)
##D
##D ---
##D <a id="logFail"></a>
##D ### logFail
##D
##D *none* **logFail**( *string* ) : *string*
##D Print a failure log message.
##D
##D **Parameter**:
##D *string* : String log message.
##D
##D **Result**:
##D *string* : Formatted log message.
##D
##D **Return**:
##D *none*
##D
##D [Function](#Functions) | [Index](#Index)
function logFail() { echo -e "\033[91mfailure:\033[0m $*" ; }
##D
##D ---
##D <a id="unsetVars"></a>
##D ### unsetVars
##D
##D *integer* **unsetVars**( *none* ) : *none*
##D Unset global variables.
##D
##D **Parameter**:
##D *none*
##D
##D **Result**:
##D *none*
##D
##D **Return**:
##D *integer* : 0 Success
##D
##D [Function](#Functions) | [Index](#Index)
function unsetVars()
{
    unset -v Source
    unset -v Destine
    return 0
}
##D
##D ---
##D <a id="_exit"></a>
##D ### _exit
##D
##D *integer* **_exit**( *none* ) : *none*
##D End log, stop libShell, deinitialize variables and exit an error code.
##D
##D **Parameter**:
##D *none*
##D
##D **Result**:
##D *none*
##D
##D **Return**:
##D *integer* : 0 Success
##D
##D [Function](#Functions) | [Index](#Index)
function _exit()
{
    local code=$([ -n "$1" ] && echo $1 || echo 0)
    logR
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
libSetup -v -l 1   || { logFail "Setup libShell.sh"      ; _exit 1 ; }
logBegin           || { logFail "Initialize Log."        ; _exit 1 ; }
##D
##D ---
##D <a id="_help"></a>
##D ### _help
##D
##D *integer* **_help**( *none* ) : *string*
##D Print an help message.
##D
##D **Parameter**:
##D *none*
##D
##D **Result**:
##D *string* : Help message.
##D
##D **Return**:
##D *integer* : 0 Success
##D
##D [Function](#Functions) | [Index](#Index)
function _help()
{
    printf "Generate Documentation from taged source files.
Script   Version: ${WHITE}$(genVersionStr ${Version[@]})${NC}
$(printLibVersion)
Syntax: $(getScriptName) [-h]
        $(getScriptName) [file]
        $(getScriptName) [options]
        $(getScriptName) [options] -- [parameters]
-h      Print help information about syntax and use.
[file]  Open file as input and save in a file with extension *.md
Options:
-i <file>       Generate documentation from input file.
-o <file>       Generate documentation into output file.
-- [parameters] Send [parameters] to libShell.
"
}
##D
##D ---
##D <a id="parseArgs"></a>
##D ### parseArgs
##D
##D *integer* **parseArgs**( *string*[] ) : *none*
##D Parse parameters from command line.
##D
##D **-h**              Print help information about syntax and use.
##D [*file*]          Open file as input and save in a file with extension *.md
##D
##D Options:
##D **-i** <*file*>       Generate documentation from input file.
##D **-o** <*file*>       Generate documentation into output file.
##D **--** [*parameters*] Send [parameters] to libShell.
##D
##D **Result**:
##D *none*
##D
##D **Return**:
##D *integer* : 0 Success
##D *integer* : 1 Failure
##D
##D [Function](#Functions) | [Index](#Index)
function parseArgs()
{
    while [ $# -gt 0 ]
    do
        case "$1" in
        -h)
            _help
            _exit 0
            ;;
        -i)
            if isArgValue "$2"
            then
                shift
                if itExist "$1" && isEmpty "${Source}"
                then
                    Source="$1"
                else
                    if ! itExist "$1" ; then logF "File ( $1 ) not found." ; fi
                    if ! isEmpty "${Source}" ; then logF "Input filename was set previously." ; fi
                    return 1
                fi
            else
                logF "Empty value for parameter -i <file>"
                return 1
            fi
            ;;
        -o)
            if isArgValue "$2"
            then
                shift
                if isEmpty "${Destine}"
                then
                    Destine="$1"
                else
                    logF "Output filename was set previously."
                    return 1
                fi
            else
                logF "Empty value for parameter -o <file>"
                return 1
            fi
            ;;
        --)
            shift
            libSetup "$@"
            break
            ;;
        *)
            if isArgValue "$1"
            then
                if itExist "$1" && isEmpty "${Source}"
                then
                    Source="$1"
                else
                    if ! itExist "$1" ; then logF "File ( $1 ) not found." ; fi
                    if ! isEmpty "${Source}" ; then logF "Input filename was set previously." ; fi
                    return 1
                fi
            else
                logF "Empty input filename parameter for $(getScriptName) [file]"
                return 1
            fi
            ;;
        esac
        shift
    done
    return 0
}
##D
##D ---
##D <a id="main"></a>
##D ### main
##D
##D *integer* **main**( *string*[] ) : *none*
##D Main shell script application.
##D
##D **Parameter**:
##D *string*[] : Parameter list from command line.
##D
##D **Result**:
##D *none*
##D
##D **Return**:
##D *integer* : 0    Success
##D *integer* : 1..N Failure
##D
##D [Function](#Functions) | [Index](#Index)
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
        logW "Empty output file, using <input>.md as output filename."
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
        # ##: Bash ;  //: C,C++,C# ;  --: SQL ;  '': VB ;  %%: LaTex
        [ -n "${line}" ] || continue
        if [[ "${line:0:4}" == "##D " ]] || \
           [[ "${line:0:4}" == "//D " ]] || \
           [[ "${line:0:4}" == "--D " ]] || \
           [[ "${line:0:4}" == "''D " ]] || \
           [[ "${line:0:4}" == "%%D " ]]
        then
            logT "${line:4}"
            echo "${line:4}" >> "${Destine}"
        elif [[ "${line:0:3}" == "##D" ]] || \
             [[ "${line:0:3}" == "//D" ]] || \
             [[ "${line:0:3}" == "--D" ]] || \
             [[ "${line:0:3}" == "''D" ]] || \
             [[ "${line:0:3}" == "%%D" ]]
        then
            logT "${line:3}"
            echo "${line:3}" >> "${Destine}"
        else
            logT "${line}"
        fi
    done < "${Source}"
    return 0
}

main "$@"
code=$?
message="Export source file ( ${Source} ) into output file ( ${Destine} )."
[ $code -eq 0 ] && logS "${message}" || logF "${message}"
_exit $code
