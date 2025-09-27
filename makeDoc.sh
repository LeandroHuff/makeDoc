#!/usr/bin/env bash
########################################
##D <h1 id="top"> makeDoc </h1>

########################################
##D <table id="description">
##D <tr> <td align="right"> <b>  Purpose</b> </td> <td align="left">:&ensp;Export Documentation from taged lines. </td> </tr>
##D <tr> <td align="right"> <b>     File</b> </td> <td align="left">:&ensp;makeDoc.sh                                                                      </td> </tr>
##D <tr> <td align="right"> <b>   Author</b> </td> <td align="left">:&ensp;Leandro - <a href="mailto:leandrohuff@programmer.net">leandrohuff@programmer.net</a> </td> </tr>
##D <tr> <td align="right"> <b>     Date</b> </td> <td align="left">:&ensp;2025-09-21                                                                           </td> </tr>
##D <tr> <td align="right"> <b>  Version</b> </td> <td align="left">:&ensp;1.0.0                                                                                </td> </tr>
##D <tr> <td align="right"> <b>Copyright</b> </td> <td align="left">:&ensp;CC01 1.0 Universal<td>                                                               </td> </tr>
##D </table> <br>

########################################
##D <b>Note</b>: Changes in this document will be discarded on next build, any changes should be made on source code documentation instead. <br>

########################################
##D <h2 id="details"> Details </h2>
##D <p>
##D Save formatted lines from source code into documentation file. <br>
##D Read source code line-by-line and save prefixed lines by tag ??D to file. <br>
##D C/C++ source code lines start with tag //D and Bash lines start with tag ##D. <br>
##D Only those lines started by tags are exportedd to documentation files. <br>
##D Mixed commented lines can co-exist at same source code, one for local documentation purpose and another to be exported to apropriate documentation file. <br>
##D All lines are documented using Markdown format, the exported document can be read by an Markdown program reader. <br>
##D </p> <br>

########################################
##D <h2 id="index"> Index </h2>
##D <table>
##D <tr> <td>               <a href="#top">Top</a>                       </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#details">Details</a>               </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#glossary">Glossary</a>             </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#constants">Constants</a>           </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#variables">Variables</a>           </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#functions">Functions</a>           </td> <td>                                                         </td> </tr>
##D <tr> <td align="right"> <a href="#logFail">logFail</a>               </td> <td> Print a failure log message                             </td> </tr>
##D <tr> <td align="right"> <a href="#unsetVars">unsetVars</a>           </td> <td> Unset global variables                                  </td> </tr>
##D <tr> <td align="right"> <a href="#_exit">_exit</a>                   </td> <td> End log, stop libShell, deinitialize variables and exit </td> </tr>
##D <tr> <td align="right"> <a href="#printHelp">printHelp</a>           </td> <td> Print an help message                                   </td> </tr>
##D <tr> <td align="right"> <a href="#saveHeaderTo">saveHeaderTo</a>     </td> <td> Save a pre formatted HTML Header                        </td> </tr>
##D <tr> <td align="right"> <a href="#saveFooterTo">saveFooterTo</a>     </td> <td> Save a pre formatted HTML Footer                        </td> </tr>
##D <tr> <td align="right"> <a href="#guiMessageBox">guiMessageBox</a>   </td> <td> Dialog box to show a message and get answer.            </td> </tr>
##D <tr> <td align="right"> <a href="#guiShowMessage">guiShowMessage</a> </td> <td> Dialog box to show a message.                         </td> </tr>
##D <tr> <td align="right"> <a href="#parseArgs">parseArgs</a>           </td> <td> Parse parameters from command line                      </td> </tr>
##D <tr> <td align="right"> <a href="#source">libShell</a>               </td> <td> Source libShell                                         </td> </tr>
##D <tr> <td align="right"> <a href="#runScript">runScript</a>           </td> <td> Main shell script application                           </td> </tr>
##D <tr> <td align="right"> <a href="#bottom">Start Script</a>           </td> <td> Start Shell Script                                      </td> </tr>
##D <tr> <td>               <a href="#bottom">Bottom</a>                 </td> <td>                                                         </td> </tr>
##D </table>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>

########################################
##D <h2 id="glossary">Glossary</h2>
##D <table>
##D <tr> <th align="left"> Use</th> <th align="left"> Description                                                      </th> </tr>
##D <tr> <td> Constants       </td> <td> Memory space for read only data                                               </td> </tr>
##D <tr> <td> Variables       </td> <td> Memory space for read/write data                                              </td> </tr>
##D <tr> <td> Functions       </td> <td> Source/Executable statement code, can be called anywhere from source code     </td> </tr>
##D <tr> <td> Parameters      </td> <td> Data passed to functions                                                      </td> </tr>
##D <tr> <td> Result          </td> <td> Functions result after execution                                              </td> </tr>
##D <tr> <td> Return          </td> <td> Allways an integer returned from function to inform success or failure        </td> </tr>
##D <tr> <td> none            </td> <td> Is similar as a void type, no parameter, no result or no return from function </td> </tr>
##D <tr> <td> char            </td> <td> One byte data type to store single characters                                 </td> </tr>
##D <tr> <td> string          </td> <td> Char vector to store a group of characters                                    </td> </tr>
##D <tr> <td> integer         </td> <td> Memory space to store ordinal numbers                                         </td> </tr>
##D <tr> <td> float           </td> <td> Memory space to store 32 bits floating point numbers                          </td> </tr>
##D <tr> <td> double          </td> <td> Memory space to store 64 bits floating point numbers                          </td> </tr>
##D <tr> <td> type[]          </td> <td> Memory vector space to store contigous data type                              </td> </tr>
##D <tr> <td> ##D             </td> <td> Bash, Zsh, Python, Perl, Ruby                                                 </td> </tr>
##D <tr> <td> //D             </td> <td> C/C++, C#, Java, JavaScript, Pascal/Object Pascal, Go, Swift, Kotlin, Rust    </td> </tr>
##D <tr> <td> --D             </td> <td> SQL, Ada, Haskell                                                             </td> </tr>
##D <tr> <td> ''D             </td> <td> Visual Basic, VBScript                                                        </td> </tr>
##D <tr> <td> %%D             </td> <td> LaTex, MATLAB                                                                 </td> </tr>
##D </table>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>

########################################
##D <br>
##D <h2 id="constants"> Constants </h2>
#
##D <i>integer</i>[] <b>numVERSION</b> : Version Number <br>
declare -a -i -r numVERSION=(1 0 0)
##D <i>integer</i>[] <b>dateVERSION</b> : Date Version Number <br>
declare -a -i -r dateVERSION=(2025 9 10)
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>

########################################
##D <br>
##D <h2 id="variables"> Variables </h2>
#
##D <i>string</i> <b>Source</b> : Source file to generate documentation from. <br>
declare Source=""
##D <i>string</i> <b>Destine</b> : Destine file to save documentation into. <br>
declare Destine=""
##D <i>integer</i> <b>exitCode</b> : Store exit code from main program. <br>
declare -i exitCode
##D <i>string</i> <b>Message</b> : Formatted message for message box. <br>
declare Message=''

##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>

########################################
##D <br>
##D <h2 id="functions"> Functions </h2>

########################################
##D <h3 id="logFail"> logFail( ) </h3>
##D <i>none</i> <b>logFail</b>( <i>string</i> <b>"$*"</b> ) : <i>string</i> <br>
##D &ensp;Send formatted failure log messages to screen. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>string</i>: <b>"$*"</b> - Message to display on screen. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>string</i>: Log message. <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>none</i> <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function logFail() { echo -e "\033[31mFailure:\033[0m $*" ; }

########################################
##D <br>
##D <h3 id="unsetVars"> unsetVars( ) </h3>
##D <i>integer</i> <b>unsetVars</b>( <i>none</i> ) : <i>none</i> <br>
##D &ensp;Unset global variables. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>none</i> <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>none</i> <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i>: <b>0</b> - Success <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function unsetVars()
{
    unset -v Source
    unset -v Destine
    unset -v title
    unset -v message
    unset -v exitCode
    return 0
}

########################################
##D <br>
##D <h3 id="_exit"> _exit( ) </h3>
##D <i>integer</i> <b>_exit</b>( <i>integer</i> <b>\$1</b> ) : <i>none</i> <br>
##D &ensp;Finish script file and return an exit code. <br>
##D <ul>
##D <li> Log runtime message.    </li>
##D <li> Finish log messages.    </li>
##D <li> Stop libShell.          </li>
##D <li> Unset global variables. </li>
##D <li> Exit an error code.     </li>
##D </ul>
##D <b>Parameter</b>: <br>
##D &ensp;<i>integer</i>: <b>$1</b> - Exit code. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>none</i> <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i>: <b>0</b> - Success <br>
##D &ensp;<i>integer</i>: <b>1..N</b> - Error code. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function _exit()
{
    declare -i code=$( [ -n "$1" ] && echo -n $1 || echo -n 0 )
    logR
    logEnd    || logFail "End log to file."
    libStop   || logFail "Stop libShell"
    unsetVars || logFail "Unset local variables"
    exit $code
}

########################################
##D <br>
##D <h3 id="printHelp"> printHelp( ) </h3>
##D <i>integer</i> <b>printHelp</b>( <i>none</i> ) : <i>string</i> <br>
##D &ensp;Print an help information. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>none</i> <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>string</i>: Help message on screen. <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i>: <b>0</b> - Success <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function printHelp()
{
    printf "Generate Documentation from taged source files.
Script   Version: ${WHITE}$(genVersionStr ${Version[@]})${NC}

Syntax: $(getScriptName) [-h]
        $(getScriptName) [file]
        $(getScriptName) [options]
        $(getScriptName) [options] -- [parameters]
-h      Print help information about syntax and use.
[file]  Open file as input and save in a file with extension *.{md|html}

Options:
-i <file>       Generate documentation from input file.
-o <file>       Generate documentation into output file.
-- [parameters] Send [parameters] to libShell.

"
    libSetup -h
    return 0
}

########################################
##D <br>
##D <h3 id="saveHeaderTo"> saveHeaderTo( )</h3>
##D <i>integer</i> <b>saveHeaderTo</b>( <i>string</i> <b>title</b> , <i>string</i> <b>file</b> ) : <i>string</i> <br>
##D &ensp;Save a pre formatted HTML Header into a target file passed by parameter. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>string</i> : <b>title</b> - HTML title, if empty, file name will be used instead. <br>
##D &ensp;<i>string</i> : <b>file</b> - Target filename. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>string</i> : Pre formatted HTML header to save into target file. <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i> : <b>0</b> - Success <br>
##D &ensp;<i>integer</i> : <b>1</b> - Error code, empty parameter or file not found. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function saveHeaderTo()
{
    local title=$( [ -n "$2" ] && echo -n "$1" || echo -n $(getName "$1") )
    local destine=$( [ -n "$2" ] && echo -n "$2" || echo -n "$1" )
    if ! [ -n "${destine}" ] || ! [ -f "${destine}" ] ; then return 1 ; fi
    printf "<!DOCTYPE html>
<html>
<head>
    <title>%s Documentation</title>
    <style>
    table, th, td {
      border: 1px solid lightgray;
      border-collapse: collapse;
    }
    th { background-color: #C0C0C0;
    }
    </style>
</head>
<body>

" ${title} > "${destine}"
    return 0
}

########################################
##D <br>
##D <h3 id="saveFooterTo"> saveFooterTo( )</h3>
##D <i>integer</i> <b>saveFooterTo</b>( <i>string</i> <b>file</b> ) : <i>string</i> <br>
##D &ensp;Save a pre defined HTML Footer into file. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>string</i> : <b>file</b> - Target file. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>string</i> : Pre formatted HTML Footer to save into a target file passed by parameter. <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i> : <b>0</b> - Success <br>
##D &ensp;<i>integer</i> : <b>1</b> - Error code, empty parameter or file not found. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function saveFooterTo()
{
    if ! [ -n "$1" ] || ! [ -f "$1" ] ; then return 1 ; fi
    printf "\n</body>\n</html>\n" >> "$1"
    return 0
}

########################################
##D <br>
##D <h3 id="guiMessageBox"> guiMessageBox( )</h3>
##D <i>integer</i> <b>guiMessageBox</b>( <i>string</i> <b>title</b> , <i>string</i> <b>text</b> , <i>string</i> <b>image</b> ) : <i>none</i> <br>
##D &ensp;Show a dialog box to search and select a file. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>string</i> : <b>title</b> - Dialog box title. <br>
##D &ensp;<i>string</i> : <b>text</b> - Dialog box text. <br>
##D &ensp;<i>string</i> : <b>image</b> - Dialog box image. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>none</i>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i> : <b>0</b> - User choose Ok. <br>
##D &ensp;<i>integer</i> : <b>1</b> - User choose Close. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function guiMessageBox
{
    local ret=1
    yad --title="$1" \
    --text="$2" \
    --image="$3" \
    --escape-ok \
    --button="Ok":0 \
    --button="Cancel":1 \
    --width=500 \
    --height=50
    ret=$?
    return $ret
}

########################################
##D <br>
##D <h3 id="guiShowMessage"> guiShowMessage( )</h3>
##D <i>integer</i> <b>guiShowMessage</b>( <i>string</i> <b>title</b> , <i>string</i> <b>text</b> , <i>string</i> <b>image</b> ) : <i>none</i> <br>
##D &ensp;Show a dialog box to search and select a file. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>string</i> : <b>title</b> - Dialog box title. <br>
##D &ensp;<i>string</i> : <b>text</b> - Dialog box text. <br>
##D &ensp;<i>string</i> : <b>image</b> - Dialog box image. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>none</i>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i> : <b>0</b> - User choose Ok. <br>
##D &ensp;<i>integer</i> : <b>1</b> - User choose Close. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function guiShowMessage
{
    local ret=1
    yad --title="$1" \
    --text="$2" \
    --image="$3" \
    --escape-ok \
    --button="Close":0 \
    --width=500 \
    --height=50
    ret=$?
    return $ret
}

########################################
##D <br>
##D <h3 id="parseArgs"> parseArgs( ) </h3>
##D <i>integer</i> <b>parseArgs</b>( <i>string</i> "<b>$@</b>" ) : <i>none</i> <br>
##D &ensp;Parse all parameters from command line. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<b>-h</b>                     - Print help information about syntax and use. <br>
##D &ensp;[<i>file</i>]                 - Open file as input and save in a file with extension *.md <br>
##D <br>
##D Options: <br>
##D &ensp;<b>-i</b> <i>file</i>         - Generate documentation from input file. <br>
##D &ensp;<b>-o</b> <i>file</i>         - Generate documentation into output file. <br>
##D &ensp;<b>--</b> [<i>parameters</i>] - Send [parameters] to libShell. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>none</i> <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i>: <b>0</b>    - Success <br>
##D &ensp;<i>integer</i>: <b>1..N</b> - Error code. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function parseArgs()
{
    while [ $# -gt 0 ]
    do
        case "$1" in
        -h)
            printHelp
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
                    if ! itExist "$1" ; then guiShowMessage "Failure" "Input file $1 not found!" "icons/failure.png" ; fi
                    if ! isEmpty "${Source}" ; then guiShowMessage "Failure" "Input filename already set previously." "icons/failure.png" ; fi
                    return 1
                fi
            else
                guiShowMessage "Failure" "Empty value for parameter -i <file>" "icons/failure.png"
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
                    guiShowMessage "Failure" "Output filename already set previously." "icons/failure.png"
                    return 1
                fi
            else
                guiShowMessage "Failure" "Empty value for parameter -o <file>" "icons/failure.png"
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
                    if ! itExist "$1" ; then guiMessageBox "Failure" "Input file $1 not found!" "icons/failure.png" ; fi
                    if ! isEmpty "${Source}" ; then guiMessageBox "Failure" "Input filename already set previously." "icons/failure.png" ; fi
                    return 1
                fi
            else
                guiMessageBox "Failure" "Option $1 not valid." "icons/failure.png"
                return 1
            fi
            ;;
        esac
        shift
    done
    return 0
}

########################################
##D <br>
##D <h3 id="source"> Source and Initialize libShell </h3>
##D <i>source</i> <b>libShell.sh</b> <br>
##D <b>libInit</b> <br>
##D <b>libSetup</b> <i>-v -l 1</i> <br>
##D <b>logBegin</b> <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
source libShell.sh || { logFail "Load libShell.sh"       ; _exit 1 ; }
libInit -d -l 1    || { logFail "Initialize libShell.sh" ; _exit 1 ; }
logBegin           || { logFail "Initialize Log."        ; _exit 1 ; }

####s####################################
##D <br>
##D <h3 id="runScript"> runScript( ) </h3>
##D <i>integer</i> <b>runScript</b>( <i>string</i> "<b>$@</b>" ) : <i>none</i> <br>
##D &ensp;Run bash script file. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D &ensp;<i>string</i>: "<b>$@</b>" - All command line parameters. <br>
##D <br>
##D <b>Result</b>: <br>
##D &ensp;<i>none</i> <br>
##D <br>
##D <b>Return</b>: <br>
##D &ensp;<i>integer</i>: <b>0</b>    - Success <br>
##D &ensp;<i>integer</i>: <b>1..N</b> - Error code. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function runScript()
{
    declare -i err=1
    # Parse command line parameters.
    parseArgs "$@" || return $?
    while ! isFile "${Source}"
    do
        logW "Empty source file or not found."
        Source=$(yad \
        --width=700 \
        --height=300 \
        --title="Select source file" \
        --file \
        --button="Ok":0 \
        --button="Cancel":1 )
        err=$?
        if [ $err -ne 0 ]
        then
            Source=$(yad \
                --width=700 \
                --height=50 \
                --title="Edit Path and Source File" \
                --entry \
                --entry-label="File: " \
                --ricon=gtk-clear \
            )
            err=$?
            if [ $err -ne 0 ]
            then
                logI "Canceled by user."
                _exit $err
            fi
        fi
    done
    logI "Source: ${Source}"
    # Check empty destine file.
    if [ -z "${Destine}" ]
    then
        logW "Empty destine file."
        Destine=$(yad \
        --width=700 \
        --height=300 \
        --title="Select destine file" \
        --file \
        --button="Ok":0 \
        --button="Cancel":1 )
        err=$?
        if [ $err -ne 0 ]
        then
            logI "Canceled by user."
            _exit $err
        fi
    fi
    logI "Destine: ${Destine}"
    # Check destine file already exist.
    if itExist "${Destine}"
    then
        logW "Destine ${Destine} already exist."
        guiMessageBox "$(getFileName "$Destine")" "Confirm to OVERWRITE file?" "icons/ok.png"
        err=$?
        if [ $err -ne 0 ]
        then
            logI "Canceled by user."
            _exit $err
        fi
    fi
    # Clear file or create a new file.
    echo -n > "${Destine}"
    # Check if file exist.
    if ! itExist "${Destine}"
    then
        logF "Could not create ${Destine} file."
        guiShowMessage "$(getFileName "${Destine}")" "Could not create file!" "icons/failure.png"
        return 1
    fi
    # For HTML file, save a header into it.
    if [ "$(getExt "${Destine}")" = "html" ]
    then
        saveHeaderTo "$(getName "${Source}")" "${Destine}"
    fi
    local counter=0
    # Run documentation procedure.
    while read -e line
    do
        counter=$((counter+1))
        logNLF "Lines complete: $counter"
        # ##: Bash ;  //: C,C++,C# ;  --: SQL ;  '': VB ;  %%: LaTex
        [ -n "${line}" ] || continue
        if [[ "${line:0:4}" == "##D " ]] || \
           [[ "${line:0:4}" == "//D " ]] || \
           [[ "${line:0:4}" == "--D " ]] || \
           [[ "${line:0:4}" == "''D " ]] || \
           [[ "${line:0:4}" == "%%D " ]]
        then
            echo "${line:4}" >> "${Destine}"
        elif [[ "${line:0:3}" == "##D" ]] || \
             [[ "${line:0:3}" == "//D" ]] || \
             [[ "${line:0:3}" == "--D" ]] || \
             [[ "${line:0:3}" == "''D" ]] || \
             [[ "${line:0:3}" == "%%D" ]]
        then
            echo "${line:3}" >> "${Destine}"
        fi
    done < "${Source}"
    # at the end, print a new line.
    echo
    # for HTML files, add a footer into it.
    if [ "$(getExt "${Destine}")" = "html" ]
    then
        saveFooterTo "${Destine}"
    fi

    return 0
}

########################################
##D <br>
##D <h3 id="bottom"> Start Shell Script </h3> <br>
##D <b>runScript</b> "<i>$@</i>" <br>
##D Call function runScript( ) and pass all parameters from command line. <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
runScript "$@"
exitCode=$?
Message="Documentation from $(getFileName ${Source}) to $(getFileName ${Destine})"
if [ $exitCode -eq 0 ]
then
    logS "${Message}"
    guiShowMessage "Success" "${Message}" "icons/success.png"
else
    logF "${Message}"
    guiShowMessage "Failure" "${Message}" "icons/failure.png"
fi
_exit $exitCode
