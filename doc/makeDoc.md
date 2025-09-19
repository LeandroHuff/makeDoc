# Markdown Document Generator

**Description**: Markdown Document Generator.
**File**       : genMarkDown.sh
**Author**     : Leandro - leandrohuff@programmer.net
**Date**       : 2025-09-16
**Version**    : 1.0.0
**Copyright**  : CC01 1.0 Universal
**Details**    : Generate a formatted markdown file from a source code.
                 Read source code line-by-line and save a documentation for tags.

## Constants

*vector integer* **Version** = *(1 0 0)*

## Variables

*string* **Source** = *''*
*string* **Destine** = *''*

## Functions

*none* **logFail**( *string* **message** ) : *string*
Print a failure log message.

*integer* **unsetVars**( **none** ) : *none*
Unset and unload global variables.

*integer* **_exit**( **none** ) : *none*
End log, stop libShell, deinitialize variables and exit an error code.

*integer* **_help**( **none** ) : *string*
Print an help message.

*integer* **parseArgs**( *vector string* **$@** ) : *none*
Parse command line parameters.

Parameter from Command Line:

**-h** | **--help**   : Print help message on screen.
**-s** | **--source** : Set a file as source of code and tags.
**-d** | **--destine**: Set a file as destine|target of markdown codes.
         **--**       : Send next parameters to libShell.

*none* **getTag**( *string* **$1** ) : *string*
Search and extract a tag from a string and print it back.

*none* **getValue**( *string* **$1** ) : *string*
Search and extract a value from a string and print it back.

*integer* **main**( *vector string* **$@** ) : *none*
Main shell script application.

### Shell Script Program

Call *main*() function and pass all command line parameters.
After return from main, call _exit() function passing the error
code returned by main().