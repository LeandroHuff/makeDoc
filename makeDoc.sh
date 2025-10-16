#!/usr/bin/env bash
########################################
##H>
##D> <h1 id="top">makeDoc</h1>

########################################
##D> <table id="description">
##D><tr><td align=right></td><td></td><td>Export Documentation from taged lines.</td></tr>
##D><tr><td align=right><b>File</b></td><td>:</td><td>makeDoc.sh</td></tr>
##D><tr><td align=right><b>Date</b></td><td>:</td><td>2025-10-05</td></tr>
##D><tr><td align=right><b>Author</b></td><td>:</td><td><a href="mailto:leandrohuff@email.com">Leandro</a></td></tr>
##D><tr><td align=right><b>Version</b></td><td>:</td><td>2.1.1 / 2025-9-30</td></tr>
##D><tr><td align=right><b>Copyright</b></td><td>:</td><td>CC01 1.0 Universal</td></tr>
##D></table>
##D><br>
########################################
##D><b>Note</b>: Changes in this document will be discarded on next build, any changes should be made on source code documentation instead. <br>

########################################
##D><h2 id="details">Details</h2>
##D><p>
##D>Save formatted lines from source code into documentation file. <br>
##D>Read source code line-by-line and save prefixed lines by tag ??D to file. <br>
##D>C/C++ source code lines start with tag //D and Bash lines start with tag ##D. <br>
##D>Only those lines started by tags are exportedd to documentation files. <br>
##D>Mixed commented lines can co-exist at same source code, one for local documentation purpose and another to be exported to apropriate documentation file. <br>
##D>All lines are documented using Markdown format, the exported document can be read by an Markdown program reader. <br>
##D></p>

########################################
##D><h2 id="index">Index</h2>
##D><table>
##D><tr><td>            <a href="#top">Top</a>                                      </td><td> </td><td>                                                     </td></tr>
##D><tr><td>            <a href="#details">Details</a>                              </td><td> </td><td>                                                     </td></tr>
##D><tr><td>            <a href="#glossary">Glossary</a>                            </td><td> </td><td>                                                     </td></tr>
##D><tr><td>            <a href="#constants">Constants</a>                          </td><td> </td><td>                                                     </td></tr>
##D><tr><td>            <a href="#variables">Variables</a>                          </td><td> </td><td>                                                     </td></tr>
##D><tr><td align=right><a href="#functions">Functions</a>                          </td><td> </td><td>                                                     </td></tr>
##D><tr><td align=right><a href="#logFail">logFail</a>                              </td><td>:</td><td>Print a failure log message.                         </td></tr>
##D><tr><td align=right><a href="#unsetVars">unsetVars</a>                          </td><td>:</td><td>Unset global variables.                              </td></tr>
##D><tr><td align=right><a href="#_exit">_exit</a>                                  </td><td>:</td><td>End log, stop libShell, unset variables and exit.    </td></tr>
##D><tr><td align=right><a href="#printHelp">printHelp</a>                          </td><td>:</td><td>Print an help message.                               </td></tr>
##D><tr><td align=right><a href="#printHelp">printHelp</a>                          </td><td>:</td><td>Print an help message.                               </td></tr>
##D><tr><td align=right><a href="#getTag">getTag</a>                                </td><td>:</td><td>Get tag name from a string until '=' symbol.         </td></tr>
##D><tr><td align=right><a href="#getValue">getValue</a>                            </td><td>:</td><td>Get tag value from a string after '=' symbol.        </td></tr>
##D><tr><td align=right><a href="#saveUserConfigToFile">saveUserConfigToFile</a>    </td><td>:</td><td>Save a pre formatted configuration into a filename.  </td></tr>
##D><tr><td align=right><a href="#loadUserConfigFromFile">loadUserConfigFromFile</a></td><td>:</td><td>Load configuration list from file or a defautl table.</td></tr>
##D><tr><td align=right><a href="#saveHeaderTo">saveHeaderTo</a>                    </td><td>:</td><td>Save a pre formatted HTML Header into a target file. </td></tr>
##D><tr><td align=right><a href="#saveFooterTo">saveFooterTo</a>                    </td><td>:</td><td>Save a pre formatted HTML Footer                     </td></tr>
##D><tr><td align=right><a href="#guiMessageBox">guiMessageBox</a>                  </td><td>:</td><td>Dialog box to show a message and get answer.         </td></tr>
##D><tr><td align=right><a href="#guiShowMessage">guiShowMessage</a>                </td><td>:</td><td>Dialog box to show a message.                        </td></tr>
##D><tr><td align=right><a href="#loadConfiguration">loadConfiguration</a>          </td><td>:</td><td>Load user configuration values from file.            </td></tr>
##D><tr><td align=right><a href="#parseArgs">parseArgs</a>                          </td><td>:</td><td>Parse parameters from command line                   </td></tr>
##D><tr><td align=right><a href="#runScript">runScript</a>                          </td><td>:</td><td>Main shell script application.                       </td></tr>
##D><tr><td align=right><a href="#source">Load libShell</a>                         </td><td>:</td><td>fileSOURCE libShell.                                 </td></tr>
##D><tr><td align=right><a href="#bottom">Start Shell Script</a>                    </td><td>:</td><td>Start Shell Script.                                  </td></tr>
##D><tr><td>            <a href="#bottom">Bottom</a>                                </td><td> </td><td>                                                     </td></tr>
##D></table>

########################################
##D><h2 id="glossary">Glossary</h2>
##D><table>
##D><tr><th align=right>Use       </th><th> </th><th align=left>Description                                                        </th></tr>
##D><tr><td align=right>Constants </td><td>:</td><td>Memory space for read only data.                                              </td></tr>
##D><tr><td align=right>Variables </td><td>:</td><td>Memory space for read/write data.                                             </td></tr>
##D><tr><td align=right>Functions </td><td>:</td><td>fileSOURCE/Executable statement code, can be called anywhere from source code.</td></tr>
##D><tr><td align=right>Parameters</td><td>:</td><td>Data passed to functions.                                                     </td></tr>
##D><tr><td align=right>Result    </td><td>:</td><td>Functions result after execution.                                             </td></tr>
##D><tr><td align=right>Return    </td><td>:</td><td>Allways an integer returned from function to inform success or failure.       </td></tr>
##D><tr><td align=right>none      </td><td>:</td><td>Is similar as a void type, no parameter, no result or no return from function.</td></tr>
##D><tr><td align=right>char      </td><td>:</td><td>One byte data type to store single characters.                                </td></tr>
##D><tr><td align=right>string    </td><td>:</td><td>Char vector to store a group of characters.                                   </td></tr>
##D><tr><td align=right>integer   </td><td>:</td><td>Memory space to store ordinal numbers.                                        </td></tr>
##D><tr><td align=right>float     </td><td>:</td><td>Memory space to store 32 bits floating point numbers.                         </td></tr>
##D><tr><td align=right>double    </td><td>:</td><td>Memory space to store 64 bits floating point numbers.                         </td></tr>
##D><tr><td align=right>type[]    </td><td>:</td><td>Memory vector space to store contigous data type.                             </td></tr>
##D></table>
##D><br>
##D><table>
##D><tr><th align=right>Tags</th><th> </th><th align=left>Programming Language Documentation                              </th></tr>
##D><tr><td align=right>##D></td><td>:</td><td>Bash, Zsh, Python, Perl, Ruby.                                             </td></tr>
##D><tr><td align=right>//D></td><td>:</td><td>C/C++, C#, Java, JavaScript, Pascal/Object Pascal, Go, Swift, Kotlin, Rust.</td></tr>
##D><tr><td align=right>--D></td><td>:</td><td>SQL, Ada, Haskell.                                                         </td></tr>
##D><tr><td align=right>''D></td><td>:</td><td>Visual Basic, VBScript.                                                    </td></tr>
##D><tr><td align=right>%%D></td><td>:</td><td>LaTex, MATLAB.                                                             </td></tr>
##D><tr><td align=right>??E></td><td>:</td><td>End, stop documentation until ??B> tag.                                    </td></tr>
##D><tr><td align=right>??B></td><td>:</td><td>Begin, start documentation until ??E> tag.                                 </td></tr>
##D><tr><td align=right>??M></td><td>:</td><td>Macro, evaluate lines to show variables values.                            </td></tr>
##D></table>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>

########################################
##D><h2 id="constants">Constants</h2>
##D><table>
##D><tr><th align=right>Type</th><th align=right>Name</th><th></th><th align=left>Value</th><th align=left>Description</th></tr>
##D><tr><td align=right> <i>integer</i>[]</td><td align=right><b>numVERSION</b></td><td>=</td><td>(2 1 1)</td><td>Version Number.</td> </tr>
declare -a -i -r numVERSION=(2 1 1)
##D><tr><td align=right> <i>integer</i>[]</td><td align=right><b>dateVERSION</b></td><td>=</td><td>(2025 9 30)</td><td>Date Version Number.</td></tr>
declare -a -i -r dateVERSION=(2025 9 30)
##D><tr><td align=right><i>integer</i></td><td align=right><b>iFILE</b></td><td>=</td><td>0</td><td>Configuration file ID.</td> </tr>
declare -i -r iFILE=0
##D><tr><td align=right><i>integer</i></td><td align=right><b>iUSER_DIR</b></td><td>=</td><td>1</td><td>ID for user directory.</td></tr>
declare -i -r iUSER_DIR=1
##D><tr><td align=right><i>integer</i></td><td align=right><b>iBASE_DIR</b></td><td>=</td><td>2</td><td>ID for base directory.</td></tr>
declare -i -r iBASE_DIR=2
##D><tr><td align=right><i>integer</i></td><td align=right><b>iAPP_DIR</b></td><td>=</td><td>3</td><td>ID for base application directory.</td></tr>
declare -i -r iAPP_DIR=3
##D><tr><td align=right><i>integer</i></td><td align=right><b>iDOC_DIR</b></td><td>=</td><td>4</td><td>ID for base documentation directory.</td></tr>
declare -i -r iDOC_DIR=4
##D><tr><td align=right><i>integer</i></td><td align=right><b>iICON_FAILURE</b></td><td>=</td><td>5</td><td>ID for Failure icon file.</td></tr>
declare -i -r iICON_FAILURE=5
##D><tr><td align=right><i>integer</i></td><td align=right><b>iICON_SUCCESS</b></td><td>=</td><td>6</td><td>ID for Success icon file.</td></tr>
declare -i -r iICON_SUCCESS=6
##D><tr><td align=right><i>integer</i></td><td align=right><b>iICON_NOK</b></td><td>=</td><td>7</td><td>ID for Not Ok icon file.</td></tr>
declare -i -r iICON_NOK=7
##D><tr><td align=right><i>integer</i></td><td align=right><b>iICON_OK</b></td><td>=</td><td>8</td><td>ID for Ok icon file.</td></tr>
declare -i -r iICON_OK=8
##D><tr><td align=right><i>integer</i></td><td align=right><b>iLOG_TARGET</b></td><td>=</td><td>9</td><td>ID for log target (screen|file).</td></tr>
declare -i -r iLOG_TARGET=9
##D><tr><td align=right><i>integer</i></td><td align=right><b>iLOG_LEVEL</b></td><td>=</td><td>10</td><td>ID for log level.</td></tr>
declare -i -r iLOG_LEVEL=10
##D><tr><td align=right><i>integer</i></td><td align=right><b>iMAX</b></td><td>=</td><td>11</td><td>Number of IDs.</td></tr>
declare -i -r iMAX=11
##D><tr><td align=right><i>integer</i>[]</td><td align=right><b>tableID</b></td><td>=</td><td>[<a href="#tableID"><sup>1</sup></a>]</td><td>IDs table.</td></tr>
declare -a -r tableID=($iFILE \
$iUSER_DIR \
$iBASE_DIR \
$iAPP_DIR \
$iDOC_DIR \
$iICON_FAILURE \
$iICON_SUCCESS \
$iICON_NOK \
$iICON_OK \
$iLOG_TARGET \
$iLOG_LEVEL)
##D><tr><td align=right><i>string</i>[]</td><td align=right><b>tableTAG</b></td><td>=</td><td>[<a href="#tableTAG"><sup>2</sup></a>]</td><td>Tags table.</td></tr>
declare -a -r tableTAG=(FILE \
USER_DIR \
BASE_DIR \
APP_DIR \
DOC_DIR \
ICON_FAILURE \
ICON_SUCCESS \
ICON_NOK \
ICON_OK \
LOG_TARGET \
LOG_LEVEL)
##D><tr><td align=right><i>string</i>[]</td><td align=right><b>tableDEFAULT</b></td><td>=</td><td>[<a href="#tableTAG"><sup>3</sup></a>]</td><td>Default values table.</td></tr>
declare -a -r tableDEFAULT=(makeDoc.cfg \
\$HOME \
dev \
makeDoc \
doc \
icons/failure.png \
icons/success.png \
icons/nok.png \
icons/ok.png \
1 \
-v)
##D></table>
##D><p id=\"tableID\"><sup>1</sup> =(0 1 2 3 4 5 6 7 8 9 10) </p>
##D><p id=\"tableTAG\"><sup>2</sup> =(FILE USER_DIR BASE_DIR APP_DIR DOC_DIR ICON_FAILURE ICON_SUCCESS ICON_NOK ICON_OK LOG_TARGET LOG_LEVEL) </p>
##D><p id=\"tableDEFAULT\"><sup>3</sup> =(makeDoc.cfg \$HOME dev makeDoc doc icons/failure.png icons/success.png icons/nok.png icons/ok.png 1 -v) </p>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>

########################################
##D><h2 id="variables">Variables</h2>
#
##D><table>
##D><tr><th align=right>Type            </th><th align=right>Name                </th> <th> </th> <th align=left>Value</th><th align=left>Description                           </th></tr>
##D><tr><td align=right> <i>boolean</i> </td><td align=right> <b>enableDOC</b>   </td> <td>=</td> <td>true            </td><td> Enable documentation.                           </td></tr>
declare enableDOC=true
##D><tr><td align=right> <i>boolean</i> </td><td align=right> <b>flagFORCE</b>   </td> <td>=</td> <td>false           </td><td> Assume yes for any question.                    </td></tr>
declare flagFORCE=false
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>fileSOURCE</b>  </td> <td>=</td> <td>''              </td><td> Source file to generate documentation from code.</td></tr>
declare fileSOURCE=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>fileDESTINE</b> </td> <td>=</td> <td>''              </td><td> Destine file to save documentation into.        </td></tr>
declare fileDESTINE=''
##D><tr><td align=right> <i>integer</i> </td><td align=right> <b>exitCODE</b>    </td> <td>=</td> <td>0               </td><td> Store exit code from main program.              </td></tr>
declare -i exitCODE=0
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>userMESSAGE</b> </td> <td>=</td> <td>''              </td><td> Formatted message for message box.              </td></tr>
declare userMESSAGE=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>tableCONFIG</b> </td> <td>=</td> <td>''              </td><td> Table for user configuration values.            </td></tr>
declare -a tableCONFIG=()
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>configFile</b>  </td> <td>=</td> <td>''              </td><td> User configuration filename.                    </td></tr>
declare configFILE=${tableDEFAULT[$iFILE]}
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>currentDir</b>  </td> <td>=</td> <td>''              </td><td> Current directory.                              </td></tr>
declare currentDIR="$PWD"
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>userDIR</b>     </td> <td>=</td> <td>''              </td><td> User home directory.                            </td></tr>
declare userDIR''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>baseDIR</b>     </td> <td>=</td> <td>''              </td><td> Base development directory.                     </td></tr>
declare baseDIR=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>appDIR</b>      </td> <td>=</td> <td>''              </td><td> Application directory.                          </td></tr>
declare appDIR=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>docDIR</b>      </td> <td>=</td> <td>''              </td><td> Documentation directory.                        </td></tr>
declare docDIR=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>iconFAIL</b>    </td> <td>=</td> <td>''              </td><td> Icon failure directory.                         </td></tr>
declare iconFAIL=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>iconSUCCESS</b> </td> <td>=</td> <td>''              </td><td> Icon success directory.                         </td></tr>
declare iconSUCCESS=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>iconNOK</b>     </td> <td>=</td> <td>''              </td><td> Icon not Ok directory.                          </td></tr>
declare iconNOK=''
##D><tr><td align=right> <i>string</i>  </td><td align=right> <b>iconOK</b>      </td> <td>=</td> <td>''              </td><td> Icon Ok directory.                              </td></tr>
declare iconOK=''
##D></table>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>

########################################
##D><h2 id="functions">Functions</h2>

########################################
##D><h3 id="logFail">logFail( )</h3>
##D><i>none</i> <b>logFail</b>( <i>string</i> <b>"$*"</b> ) : <i>string</i> <br>
##D>&ensp;Send formatted failure log messages to screen. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i>: <b>"$*"</b> - userMESSAGE to display on screen. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i>: Log message. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function logFail() { echo -e "\033[31mFailure:\033[0m $*" ; }

########################################
##D><br>
##D><h3 id="unsetVars">unsetVars( )</h3>
##D><i>integer</i> <b>unsetVars</b>( <i>none</i> ) : <i>none</i> <br>
##D>&ensp;Unset global variables. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i>: <b>0</b> - Success <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function unsetVars()
{
    unset -v fileSOURCE
    unset -v fileDESTINE
    unset -v title
    unset -v message
    unset -v exitCODE
    return 0
}

########################################
##D><br>
##D><h3 id="_exit">_exit( )</h3>
##D><i>integer</i> <b>_exit</b>( <i>integer</i> <b>\$1</b> ) : <i>none</i> <br>
##D>&ensp;Finish script file and return an exit code. <br>
##D><ul>
##D><li> Log runtime message.    </li>
##D><li> Finish log messages.    </li>
##D><li> Stop libShell.          </li>
##D><li> Unset global variables. </li>
##D><li> Exit an error code.     </li>
##D></ul>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>integer</i>: <b>$1</b> - Exit code. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i>: <b>0</b> - Success <br>
##D>&ensp;<i>integer</i>: <b>1..N</b> - Error code. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
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
##D><br>
##D><h3 id="printHelp">printHelp( )</h3>
##D><i>integer</i> <b>printHelp</b>( <i>none</i> ) : <i>string</i> <br>
##D>&ensp;Print an help information. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i>: Help message on screen. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i>: <b>0</b> - Success <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function printHelp()
{
    printf "Generate Documentation from taged source files.
Script Version: ${WHITE}$(genVersionStr ${numVERSION[@]})${NC}
Script    Date: ${WHITE}$(genDateVersionStr ${dateVERSION[@]})${NC}

Syntax: $(getScriptName) [-h]
        $(getScriptName) [file]
        $(getScriptName) [options]
        $(getScriptName) [options] -- [parameters]
-h      Print help information about syntax and use.
[file]  Open file as input and save in a file with extension *.{md|html}

Options:
-y              Assume yes for any question.
-i <file>       Generate documentation from input file.
-o <file>       Generate documentation into output file.
-- [parameters] Send [parameters] to libShell.

"
    libSetup -h
    return 0
}

########################################
##D><br>
##D><h3 id="getTag">getTag( )</h3>
##D><i>none</i> <b>getTag</b>( <i>string</i> <b>line</b> ) : <i>string</i> <br>
##D>&ensp;Get tag name from a string until '=' symbol. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>line</b> - Get the tag name from string line. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i> : Echoe the 'tag' name from line string. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function getTag() { echo -n "${1%=*}" ; }

########################################
##D><br>
##D><h3 id="getValue">getValue( )</h3>
##D><i>none</i> <b>getValue</b>( <i>string</i> <b>line</b> ) : <i>string</i> <br>
##D>&ensp;Get tag value from a string after '=' symbol. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>line</b> - Get the tag value from string line. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i> : Echoe the 'tag' value from line string. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function getValue() { echo -n "${1##*=}" ; }

########################################
##D><br>
##D><h3 id="saveUserConfigToFile">saveUserConfigToFile( )</h3>
##D><i>integer</i> <b>saveUserConfigToFile</b>( <i>string</i> <b>filename</b> ) : <i>string</i> <br>
##D>&ensp;Save a pre formatted configuration into a filename. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>filename</b> - Filename to save user configuration content. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i> : Save a pre formatted configuration into a filename. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i> : <b>0..N</b> - Return code. <br>
##D>&ensp;<i>integer</i> : <b>0</b> - Success <br>
##D>&ensp;<i>integer</i> : <b>1</b> - Empty parameter. <br>
##D>&ensp;<i>integer</i> : <b>2</b> - Could not save to file. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function saveUserConfigToFile()
{
    local configVERSION=(1 0 0 )
    local configDATE=(2025 9 27)
    local err=0
    local file="$1"
    if ! [ -n "${file}" ]
    then
        logF "Empty parameter to saveUserConfigToFile()"
        return 1
    fi
##E>
cat << EOT > "${file}" || err=$?
# HTML Documentation.
# Syntax: [-y] makeDoc.sh -i makeDoc.cfg -o makeDoc.html

##D><h1>makeDoc User Configuration</h1>

##D><table>
##D><tr><td align=right>                </td><td> </td><td>User configuration file for makeDoc application.                            </td></tr>
##D><tr><td align=right><b>     File</b></td><td>:</td><td>makeDoc.cfg                                                                 </td></tr>
##D><tr><td align=right><b>File Date</b></td><td>:</td><td>$(getDate)                                                                  </td></tr>
##D><tr><td align=right><b>  Version</b></td><td>:</td><td>$(genVersionStr ${configVERSION[@]}) / $(genDateVersionStr ${configDATE[@]})</td></tr>
##D><tr><td align=right><b>   Author</b></td><td>:</td><td><a href="mailto:leandrohuff@email.com">Leandro</a>                          </td></tr>
##D><tr><td align=right><b>Copyright</b></td><td>:</td><td>CC01 1.0 Universal                                                          </td></tr>
##D></table> <br>

##D><p>
##D><b>Note</b>: Changes in this documentation will be discarded on next build, any changes should be made in source code documentation instead. <br>
##D></p>

##D><h2>Details</h2>
##D><p>
##D>Export HTML documentation. <br>
##D>Syntax: <b>makeDoc.sh</b> [<b>-y</b>] <b>-i</b> <i>filename.cfg</i> <b>-o</b> <i>filename[.cfg].html</i> <br>
##D></p>

##D><h2> Configuration </h2>

# 1.
##D><table>
##D><tr><th align=right>Tag</th><th> </th><th align=left>Value</th></tr>
##D><tr><td align=right><b>FILE</b></td><td>=</td><td><i>${tableCONFIG[$iFILE]}</i></td></tr>
FILE=${tableCONFIG[$iFILE]}

# 2.
##D><tr><td align=right><b>USER_DIR</b></td><td>=</td><td><i>${tableCONFIG[$iUSER_DIR]}</i></td></tr>
USER_DIR=${tableCONFIG[$iUSER_DIR]}

# 3.
##D><tr><td align=right><b>BASE_DIR</b></td><td>=</td><td><i>${tableCONFIG[$iBASE_DIR]}</i></td></tr>
BASE_DIR=${tableCONFIG[$iBASE_DIR]}

# 4.
##D><tr><td align=right><b>APP_DIR</b></td><td>=</td><td><i>${tableCONFIG[$iAPP_DIR]}</i></td></tr>
APP_DIR=${tableCONFIG[$iAPP_DIR]}

# 5.
##D><tr><td align=right><b>DOC_DIR</b></td><td>=</td><td><i>${tableCONFIG[$iDOC_DIR]}</i></td></tr>
DOC_DIR=${tableCONFIG[$iDOC_DIR]}

# 6.
##D><tr><td align=right><b>ICON_FAILURE</b></td><td>=</td><td><i>${tableCONFIG[$iICON_FAILURE]}</i></td></tr>
ICON_FAILURE=${tableCONFIG[$iICON_FAILURE]}

# 7.
##D><tr><td align=right><b>ICON_SUCCESS</b></td><td>=</td><td><i>${tableCONFIG[$iICON_SUCCESS]}</i></td></tr>
ICON_SUCCESS=${tableCONFIG[$iICON_SUCCESS]}

# 8.
##D><tr><td align=right><b>ICON_NOK</b></td><td>=</td><td><i>${tableCONFIG[$iICON_NOK]}</i></td></tr>
ICON_NOK=${tableCONFIG[$iICON_NOK]}

# 9.
##D><tr><td align=right><b>ICON_OK</b></td><td>=</td><td><i>${tableCONFIG[$iICON_OK]}</i></td></tr>
ICON_OK=${tableCONFIG[$iICON_OK]}

# 10.
##D><tr><td align=right><b>LOG_TARGET</b></td><td>=</td><td><i>${tableCONFIG[$iLOG_TARGET]}</i></td></tr>
LOG_TARGET=${tableCONFIG[$iLOG_TARGET]}

# 11.
##D><tr><td align=right><b>LOG_LEVEL</b></td><td>=</td><td><i>${tableCONFIG[$iLOG_LEVEL]}</i></td></tr>
LOG_LEVEL=${tableCONFIG[$iLOG_LEVEL]}
EOT
##B>
    local message="Save user configuration into ${file} file."
    if [ $err -eq 0 ]
    then
        logS "${message}"
        return 0
    else
        logF "${message}"
        return 2
    fi
}

########################################
##D><br>
##D><h3 id="loadUserConfigFromFile">loadUserConfigFromFile( )</h3>
##D><i>integer</i> <b>loadUserConfigFromFile</b>( <i>string</i> <b>filename</b> ) : <i>string</i> <br>
##D>&ensp;Load configuration list from a file or a default configuration table. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>filename</b> - Filename to load user configuration content. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i>[] <b>tableCONFIG</b> : Store configuration from filename. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i> : <b>0..N</b> - Return code. <br>
##D>&ensp;<i>integer</i> : <b>0</b> - Success <br>
##D>&ensp;<i>integer</i> : <b>1</b> - Empty parameter or file acces is not available. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function loadUserConfigFromFile()
{
    local file="$1"
    if ! [ -f "${file}" ] ; then
        tableCONFIG=("${tableDEFAULT[@]}")
        logE "Unable to load configuration from file ${file}, using defaults."
        return 1
    fi
    local tag value
    for id in "${tableID[@]}" ; do
        value=''
        # search tag
        while read -e line ; do
            # do not compute empty lines
            if ! [ -n "${line}" ] ; then continue ; fi
            # do not compute commented lines
            if [ ${line:0:1} = "#" ] ; then continue ; fi
            # take tag from line
            tag="$(getTag "${line}")"
            # compare with searching tag name
            [[ "${tag}" == "${tableTAG[$id]}" ]] || continue
            # take value from line
            value="$(getValue "${line}")"
            break
        done < "${file}"
        # not found, use default
        [ -n "$value" ] || value="${tableDEFAULT[$id]}"
        # add item into config table
        [ -n "${tableCONFIG[*]}" ] && tableCONFIG+=("${value}") || tableCONFIG=("${value}")
    done
    return 0
}

########################################
##D><br>
##D><h3 id="saveHeaderTo">saveHeaderTo( )</h3>
##D><i>integer</i> <b>saveHeaderTo</b>( <i>string</i> <b>title</b> , <i>string</i> <b>file</b> ) : <i>string</i> <br>
##D>&ensp;Save a pre formatted HTML Header into a target file passed by parameter. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>title</b> - HTML title, if empty, file name will be used instead. <br>
##D>&ensp;<i>string</i> : <b>file</b> - Target filename. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i> : Pre formatted HTML header to save into target file. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i> : <b>0</b> - Success <br>
##D>&ensp;<i>integer</i> : <b>1</b> - Error code, empty parameter or file not found. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function saveHeaderTo()
{
    local title=$( [ -n "$2" ] && echo -n "$1" || echo -n "$(getName "$1")" )
    local destine=$( [ -n "$2" ] && echo -n "$2" || echo -n "$1" )
    if ! [ -n "${destine}" ] || ! [ -f "${destine}" ] ; then return 1 ; fi
    printf "<!DOCTYPE html>
<html>
<head>
    <title>%s Documentation</title>
    <style>
    table {
        border: hidden;
        border-collapse: collapse;
        padding-left: 5px;
        padding-right: 5px;
    }
    th {
        background-color: #E0E0E0;
    }
    </style>
</head>
<body>

" ${title} > "${destine}"
    return 0
}

########################################
##D><br>
##D><h3 id="saveFooterTo">saveFooterTo( )</h3>
##D><i>integer</i> <b>saveFooterTo</b>( <i>string</i> <b>file</b> ) : <i>string</i> <br>
##D>&ensp;Save a pre defined HTML Footer into file. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>file</b> - Target file. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>string</i> : Pre formatted HTML Footer to save into a target file passed by parameter. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i> : <b>0</b> - Success <br>
##D>&ensp;<i>integer</i> : <b>1</b> - Error code, empty parameter or file not found. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function saveFooterTo()
{
    if ! [ -n "$1" ] || ! [ -f "$1" ] ; then return 1 ; fi
    printf "\n</body>\n</html>\n" >> "$1"
    return 0
}

########################################
##D><br>
##D><h3 id="guiMessageBox">guiMessageBox( )</h3>
##D><i>integer</i> <b>guiMessageBox</b>( <i>string</i> <b>title</b> , <i>string</i> <b>text</b> , <i>string</i> <b>image</b> ) : <i>none</i> <br>
##D>&ensp;Dialog box to show a message and get answer. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>title</b> - Dialog box title. <br>
##D>&ensp;<i>string</i> : <b>text</b> - Dialog box text. <br>
##D>&ensp;<i>string</i> : <b>image</b> - Dialog box image. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i> : <b>0</b> - User choose Ok. <br>
##D>&ensp;<i>integer</i> : <b>1</b> - User choose Close. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
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
##D><br>
##D><h3 id="guiShowMessage">guiShowMessage( )</h3>
##D><i>integer</i> <b>guiShowMessage</b>( <i>string</i> <b>title</b> , <i>string</i> <b>text</b> , <i>string</i> <b>image</b> ) : <i>none</i> <br>
##D>&ensp;Dialog box to show a message. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>title</b> - Dialog box title. <br>
##D>&ensp;<i>string</i> : <b>text</b> - Dialog box text. <br>
##D>&ensp;<i>string</i> : <b>image</b> - Dialog box image. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i> : <b>0</b> - User choose Ok. <br>
##D>&ensp;<i>integer</i> : <b>1</b> - User choose Close. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function guiShowMessage
{
    local ret=1
    yad --title="$1" \
    --text="$2" \
    --image="$3" \
    --escape-ok \
    --button="Close":0 \
    --width=500 \
    --height=50 \
    ret=$?
    return $ret
}

########################################
##D><br>
##D><h3 id="loadConfiguration">loadConfiguration( )</h3>
##D><i>integer</i> <b>loadConfiguration</b>( <i>string</i> <b>file</b> ) : <i>none</i> <br>
##D>&ensp;Load user configuration values from file. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> : <b>file</b> - Configuration filename. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i> : <b>0</b> - Success <br>
##D>&ensp;<i>integer</i> : <b>1..N</b> -Failure <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function loadConfiguration()
{
    local err=0

    loadUserConfigFromFile "${configFILE}"
    saveUserConfigToFile "${configFILE}"

    userDIR=$(eval echo "${tableCONFIG[$iUSER_DIR]}")
    baseDIR="${userDIR}/${tableCONFIG[$iBASE_DIR]}"
    appDIR="${baseDIR}/${tableCONFIG[$iAPP_DIR]}"
    docDIR="${appDIR}/${tableCONFIG[$iDOC_DIR]}"
    iconFAIL="${appDIR}/${tableCONFIG[$iICON_FAILURE]}"
    iconSUCCESS="${appDIR}/${tableCONFIG[$iICON_SUCCESS]}"
    iconNOK="${appDIR}/${tableCONFIG[$iICON_NOK]}"
    iconOK="${appDIR}/${tableCONFIG[$iICON_OK]}"

    return 0
}

########################################
##D><br>
##D><h3 id="parseArgs">parseArgs( )</h3>
##D><i>integer</i> <b>parseArgs</b>( <i>string</i> "<b>$@</b>" ) : <i>none</i> <br>
##D>&ensp;Parse all parameters from command line. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<b>-h</b>                     - Print help information about syntax and use. <br>
##D>&ensp;[<i>file</i>]                 - Open file as input and save in a file with extension *.md <br>
##D><br>
##D>Options: <br>
##D>&ensp;<b>-i</b> <i>file</i>         - Generate documentation from input file. <br>
##D>&ensp;<b>-o</b> <i>file</i>         - Generate documentation into output file. <br>
##D>&ensp;<b>--</b> [<i>parameters</i>] - Send [parameters] to libShell. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i>: <b>0</b>    - Success <br>
##D>&ensp;<i>integer</i>: <b>1..N</b> - Error code. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function parseArgs()
{
    while [ $# -gt 0 ]
    do
        case "$1" in
        -h)
            printHelp
            _exit 0
            ;;
        -y) flagFORCE=true ;;
        -i)
            if isArgValue "$2"
            then
                shift
                if itExist "$1"
                then
                    if isEmpty "${fileSOURCE}" || $flagFORCE
                    then
                        fileSOURCE="$1"
                        logD "fileSOURCE: ${fileSOURCE}"
                    else
                        guiShowMessage "Failure" "Input filename already set previously." "${iconFAILURE}"
                        return 1
                    fi
                else
                    guiShowMessage "Failure" "Input file $1 not found!" "icons/failure.png"
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
                if isEmpty "${fileDESTINE}"  || $flagFORCE
                then
                    fileDESTINE="$1"
                    logD "fileDESTINE: ${fileDESTINE}"
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
                if itExist "$1"
                then
                    if isEmpty "${fileSOURCE}" || $flagFORCE
                    then
                        fileSOURCE="$1"
                        logD "fileSOURCE: ${fileSOURCE}"
                    else
                        guiShowMessage "Failure" "Input filename already set previously." "icons/failure.png"
                        return 1
                    fi
                else
                    guiShowMessage "Failure" "Input file $1 not found!" "icons/failure.png"
                fi
            else
                guiShowMessage "Failure" "Empty value for parameter -i <file>" "icons/failure.png"
                return 1
            fi
            ;;
        esac
        shift
    done
    return 0
}

####s####################################
##D><br>
##D><h3 id="runScript">runScript( )</h3>
##D><i>integer</i> <b>runScript</b>( <i>string</i> "<b>$@</b>" ) : <i>none</i> <br>
##D>&ensp;Run bash script file. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i>: "<b>$@</b>" - All command line parameters. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i>: <b>0</b>    - Success <br>
##D>&ensp;<i>integer</i>: <b>1..N</b> - Error code. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
function runScript()
{
    declare -i err=0

    loadConfiguration
    libSetup "${tableCONFIG[$iLOG_LEVEL]}" -l "${tableCONFIG[$iLOG_TARGET]}"
    parseArgs "$@" || return 1

    logD "-----------------------------------------------"
    logD "Variables from file: ${configFILE}"
    logD "-----------------------------------------------"
    logD "Config  file: ${tableCONFIG[$iFILE]}"
    logD "Current  dir: ${currentDIR}"
    logD "User     dir: ${userDIR}"
    logD "Base     dir: ${baseDIR}"
    logD "App      dir: ${appDIR}"
    logD "Doc      dir: ${docDIR}"
    logD "Temp     dir: $libTMP}"
    logD "Log  to file: ${logFILE}"
    logD "Icon    fail: ${iconFAIL}"
    logD "Icon success: ${iconSUCCESS}"
    logD "Icon  not ok: ${iconNOK}"
    logD "Icon      ok: ${iconOK}"
    logD "Log   target: ${tableCONFIG[$iLOG_LEVEL]}"
    logD "Log    level: ${tableCONFIG[$iLOG_TARGET]}"
    logD "-----------------------------------------------"

    while ! isFile "${fileSOURCE}"
    do
        logW "Empty source file or not found."
        # Search source file.
        fileSOURCE=$(yad \
        --width=700 \
        --height=300 \
        --title="Select source file" \
        --file \
        --button="Ok":0 \
        --button="Cancel":1 )
        err=$?
        if [ $err -ne 0 ]
        then
            # Edit source file.
            fileSOURCE=$(yad \
                --width=700 \
                --height=50 \
                --title="Edit Path and Source file" \
                --entry \
                --entry-label="File: " \
                --entry-text="${currentDIR}/" \
                --ricon=gtk-clear )
            err=$?
            if [ $err -ne 0 ]
            then
                logI "Canceled by user."
                _exit $err
            fi
        fi
    done
    logI "Source: ${fileSOURCE}"
    # Check empty destine file.
    if [ -z "${fileDESTINE}" ]
    then
        logW "Empty destine file."
        # Search destine file.
        fileDESTINE=$(yad \
        --width=700 \
        --height=300 \
        --title="Select Destine file" \
        --file \
        --button="Ok":0 \
        --button="Cancel":1 )
        err=$?
        if [ $err -ne 0 ] || [ -z "${fileDESTINE}" ] || ! [ -f "${fileDESTINE}" ]
        then
            fileDESTINE=$([ -n "${fileDESTINE}" ] && echo -n "${fileDESTINE}" || echo -n "doc/")
            # Edit destine file.
            fileDESTINE=$(yad \
                --width=700 \
                --height=50 \
                --title="Edit Path and Destine File" \
                --entry= \
                --entry-label="File: " \
                --entry-text="${fileDESTINE}/" \
                --ricon=gtk-clear )
            err=$?
            if [ $err -ne 0 ]
            then
                logI "Canceled by user."
                _exit $err
            fi
        fi
    fi
    logI "Destine: ${fileDESTINE}"
    # Check destine file already exist.
    if ! $flagFORCE && itExist "${fileDESTINE}"
    then
        logW "Destine ${fileDESTINE} already exist."
        guiMessageBox "$(getFileName "$fileDESTINE")" "Confirm to OVERWRITE file?" "${iconOK}"
        err=$?
        if [ $err -ne 0 ]
        then
            logI "Canceled by user."
            _exit $err
        fi
    fi
    # Clear file or create a new file.
    echo -n > "${fileDESTINE}"
    # Check if file exist.
    if ! itExist "${fileDESTINE}"
    then
        logF "Could not create ${fileDESTINE} file."
        guiShowMessage "$(getFileName "${fileDESTINE}")" "Could not create file!" "${iconFAIL}"
        return 1
    fi
    # For HTML file, first save a header into documentation file.
    if [ "$(getExt "${fileDESTINE}")" = "html" ]
    then
        saveHeaderTo "$(getName "${fileSOURCE}")" "${fileDESTINE}"
    fi
    local counter=0
    # Run export documentation procedure.
    while read -e line
    do
        counter=$((counter+1))
        logNLF "         (${WHITE}$counter${NC}) lines completed."
        # skip empty lines
        [ -n "${line}" ] || continue
        userMESSAGE=''
        case "${line:0:4}" in
        "##B>" | "//B>" | "--B>" | "''B>") enableDOC=true  ; continue ;;
        "##E>" | "//E>" | "--E>" | "''E>") enableDOC=false ; continue ;;
        *)  if ! $enableDOC ; then continue ; fi
            case "${line:0:5}" in
                "##D> " | "//D> " | "%%D> " | "''D> " | "--D>") printf -v userMESSAGE "%s" "${line:5}" ;;
                "##M> " | "//M> " | "%%M> " | "''M>"  | "--M>") printf -v userMESSAGE "%s" "$(eval ${line:5})" ;;
                *)  case "${line:0:4}" in
                        "##D>"  | "//D>"  | "%%D>"  | "''D>" | "--D>") printf -v userMESSAGE "%s" "${line:4}" ;;
                        "##M>"  | "//M>"  | "%%M>"  | "''M>" | "--M>") printf -v userMESSAGE "%s" "$(eval ${line:4})" ;;
                        *) continue ;;
                    esac
                    ;;
            esac
            ;;
        esac
        echo "${userMESSAGE}" >> "${fileDESTINE}"
    done < "${fileSOURCE}"
    # at the end, print a new line for logNLF "".
    echo
    # for HTML files, add a footer at the end of documentation file.
    if [ "$(getExt "${fileDESTINE}")" = "html" ]
    then
        saveFooterTo "${fileDESTINE}"
    fi

    return $err
}

########################################
##D><br>
##D><hr>

########################################
##D><h3 id="source">Load and Initialize libShell</h3>
##D><i>source</i> <b>libShell.sh</b> <br>
##D><b>libInit</b> <i>-v -l 1</i> <br>
##D><b>logBegin</b> <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
source libShell.sh || { logFail "Load libShell.sh"       ; _exit 1 ; }
libInit -v -l 1    || { logFail "Initialize libShell.sh" ; _exit 1 ; }
logBegin           || { logFail "Initialize Log."        ; _exit 1 ; }

########################################
##D><br>
##D><h3 id="bottom">Start Shell Script</h3>
##D><b>runScript</b> "<i>$@</i>" <br>
##D>Call function runScript( ) and pass all parameters from command line. <br>
##D><br>
##D>[<a href="#top">Top</a>] | [<a href="#index">Index</a>] | [<a href="#bottom">Bottom</a>] <br>
##F>
runScript "$@"
exitCODE=$?

userMESSAGE="Documentation from $(getFileName ${fileSOURCE}) to $(getFileName ${fileDESTINE})"
if [ $exitCODE -eq 0 ]
then
    logS "${userMESSAGE}"
    if ! $flagFORCE ; then guiShowMessage "Success" "${userMESSAGE}" "icons/success.png" ; fi
else
    logF "${userMESSAGE}"
    if ! $flagFORCE ; then guiShowMessage "Failure" "${userMESSAGE}" "icons/failure.png" ; fi
fi

_exit $exitCODE
