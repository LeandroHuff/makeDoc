# Markdown Document Generator

**Description**: Markdown Document Generator
**File**       : makeDoc.sh
**Author**     : Leandro - leandrohuff@programmer.net
**Date**       : 2025-09-16
**Version**    : 1.0.0
**Copyright**  : CC01 1.0 Universal
**Details**    :
Save formatted markdown lines from source code into destine file.
Read source code line-by-line and save prefixed lines by "/D" to a file.
C/C++ source code lines start with "//D" tag and Shell Script lines start with "#/D" tags.
Only lines started with apropriate tags are saved into respective documentation files.

## Constants

*integer*[] **Version** = (*1 0 0*)

## Variables

*string* **Source** = *''*
*string* **Destine** = *''*

## Functions

### logFail()

**Function**: *none* **logFail**( *string* ) : *string*
Print a failure log message.
**Parameter**:
*string*    String log message.
**Result**:
*string*    Formatted log message.
**Return**:
*none*

### unsetVars()

**Function**: *integer* **unsetVars**( *none* ) : *none*
Unset global variables.
**Parameter**:
*none*
**Result**:
*none*
**Return**:
*integer* 0 Success

### _exit()

**Function**: *integer* **_exit**( *none* ) : *none*
End log, stop libShell, deinitialize variables and exit an error code.
**Parameter**:
*none*
**Result**:
*none*
**Return**:
*integer* 0 Success

### _help()

**Function**: *integer* **_help**( *none* ) : *string*
Print an help message.
**Parameter**:
*none*
**Result**:
*string*    Help message.
**Return**:
*integer* 0 Success

### parseArgs()

**Function**: *integer* **parseArgs**( *string*[] ) : *none*
Parse parameters from command line.
**Parameter**:
**-h** | **--help**            : Print help message on screen.
**-s** | **--source**  <*file*>: Set a file as source of code and tags.
**-d** | **--destine** <*file*>: Set a file as destine|target of markdown codes.
     **--**              : Send next parameters to libShell.
**Result**:
*none*
**Return**:
*integer* 0 Success
*integer* 1 Failure

### getTag()

**Function**: *none* **getTag**( *string* ) : *string*
Search and extract a tag from a string and print it back.
**Parameter**:
*string*    String to search for a tag name.
**Result**:
*string*    Tag name.
**Return**:
*none*

### getValue()

**Function**: *none* **getValue**( *string* ) : *string*
Search and extract a value from a string and print it back.
**Parameter**:
*string*    String to search for a tag value.
**Result**:
*string*    Tag value.
**Return**:
*none*

### main()

**Function**: *integer* **main**( *string*[] ) : *none*
Main shell script application.
**Parameter**:
*string*[]  Parameter list from command line.
**Result**:
*none*
**Return**:
*integer* 0    Success
*integer* 1..N Failure

### Shell Script Program

Call **main**() function.
Pass all command line parameters to main.
