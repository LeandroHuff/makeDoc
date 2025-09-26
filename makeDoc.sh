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
##D <tr> <td>               <a href="#top">Top</a>             </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#details">Details</a>     </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#glossary">Glossary</a>   </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#constants">Constants</a> </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#variables">Variables</a> </td> <td>                                                         </td> </tr>
##D <tr> <td>               <a href="#functions">Functions</a> </td> <td>                                                         </td> </tr>
##D <tr> <td align="right"> <a href="#logFail">logFail</a>     </td> <td> Print a failure log message                             </td> </tr>
##D <tr> <td align="right"> <a href="#unsetVars">unsetVars</a> </td> <td> Unset global variables                                  </td> </tr>
##D <tr> <td align="right"> <a href="#_exit">_exit</a>         </td> <td> End log, stop libShell, deinitialize variables and exit </td> </tr>
##D <tr> <td align="right"> <a href="#printHelp">printHelp</a> </td> <td> Print an help message                                   </td> </tr>
##D <tr> <td align="right"> <a href="#parseArgs">parseArgs</a> </td> <td> Parse parameters from command line                      </td> </tr>
##D <tr> <td align="right"> <a href="#barGraph">barGraph</a>   </td> <td> Draw a prograssive line counter bar graph               </td> </tr>
##D <tr> <td align="right"> <a href="#source">libShell</a>     </td> <td> Source libShell                                         </td> </tr>
##D <tr> <td align="right"> <a href="#runScript">runScript</a> </td> <td> Main shell script application                           </td> </tr>
##D <tr> <td align="right"> <a href="#bottom">Start Script</a> </td> <td> Start Shell Script                                      </td> </tr>
##D <tr> <td>               <a href="#bottom">Bottom</a>       </td> <td>                                                         </td> </tr>
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
##D <i>integer</i>[] <b>numVERSION</b> = ( <i>1 0 0</i> ) <br>
declare -a -i -r numVERSION=(1 0 0)
##D <i>integer</i>[] <b>dateVERSION</b> = ( <i>2025 9 21</i> ) <br>
declare -a -i -r dateVERSION=(2025 9 10)
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>

########################################
##D <br>
##D <h2 id="variables"> Variables </h2>
#
##D <i>string</i> <b>Source</b> = <i>''</i> <br>
declare Source=""
##D <i>string</i> <b>Destine</b> = <i>''</i> <br>
declare Destine=""
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
    unset -v exitCode
    return 0
}

########################################
##D <br>
##D <h3 id="_exit"> _exit( ) </h3>
##D <i>integer</i> <b>_exit</b>( <i>integer</i> <b>\$1</b> ) : <i>none</i> <br>
##D Finish script file and return an exit code. <br>
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
[file]  Open file as input and save in a file with extension *.md

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

########################################
##D <br>
##D <h3 id="barGraph"> barGraph( )</h3>
##D <br>
##D <i>none</i> <b>barGraph</b>( <i>integer</i> <b>counter</b> ) : <i>string</i> <br>
##D Draw a prograssive line counter bar graph. <br>
##D <br>
##D <b>Parameter</b>: <br>
##D <i>integer</i> : <b>counter</b> - Progress counter. <br>
##D <br>
##D <b>Result</b>: <br>
##D <i>string</i> : Draw a progressive counter bar graph accorgin to lines read for file. <br>
##D <br>
##D <b>Return</b>: <br>
##D <i>none</i> <br>
##D <br>
##D <a href="#top"> Top </a> | <a href="#index"> Index </a> | <a href="#bottom"> Bottom </a> <br>
function barGraph()
{
    local counter=$1
    # print a green '*'
    printf "${HGREEN}*${NC}"
    # print [N] each 10 and '|' each 5
    if   [ $((counter % 10)) -eq 0 ] ; then printf "[%4d]" $counter
    elif [ $((counter %  5)) -eq 0 ] ; then printf '|' ; fi
    # echo a new line each 50
    if  [ $((counter % 50)) -eq 0 ] ; then echo ; fi
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
libInit            || { logFail "Initialize libShell.sh" ; _exit 1 ; }
libSetup -v -l 1   || { logFail "Setup libShell.sh"      ; _exit 1 ; }
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
    local counter=0
    while read -e line
    do
        let counter++
        barGraph $counter
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
    echo
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
message="Export from source file ( ${Source} ) to output file ( ${Destine} )."
[ $exitCode -eq 0 ] && logS "${message}" || logF "${message}"
_exit $exitCode
