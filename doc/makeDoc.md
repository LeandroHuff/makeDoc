# makeDoc

| Type/Name | Description                           |
|----------:|:--------------------------------------|
|           | Export Documentation from taged lines |
| File      | makeDoc.sh                            |
| Author    | Leandro - leandrohuff@programmer.net  |
| Date      | 2025-09-16                            |
| Version   | 1.0.0                                 |
| Copyright | CC01 1.0 Universal                    |

---
## Details

Save formatted lines from source code into documentation file.
Read source code line-by-line and save prefixed lines by tag ??D to file.
C/C++ source code lines start with tag //D and Bash lines start with tag ##D.
Only those lines started by tags are exportedd to documentation files.
Mixed commented lines can co-exist at same source code, one for local documentation purpose and another to be exported to apropriate documentation file.
All lines are documented using Markdown format, the exported document can be read by an Markdown program reader.

---
## Documentation Format

```
??D # fileName
??D
??D | Type/Name | Description              |
??D |----------:|:-------------------------|
??D |           | Source Code Description  |
??D | File      | fileName.sh              |
??D | Author    | Name - author@e-Mail     |
??D | Date      | YYYY-MM-DD               |
??D | Version   | Version.Release.Features |
??D | Copyright | Copyright description    |
??D
??D ---
??D ## Index
??D
??D [Constants](#Constants)
??D [Variables](#Variables)
??D [Functions](#Functions)
??D
??D ---
??D <a id="Constants"></a>
??D ## Constants
??D
??D *type*[] **Name** = *value*
??D
??D [Index](#Index)
??D
??D ---
??D <a id="Variables"></a>
??D ## Variables
??D
??D *type* **Name** = *value*
??D
??D [Index](#Index)
??D
??D ---
??D <a id="Functions"></a>
??D ## Functions
??D
??D | Function Index              | Brief Description       |
??D |----------------------------:|:------------------------|
??D | [FunctionOne](#FunctionOne) | FunctionOne description |
??D | [FunctionTwo](#FunctionTwo) | FunctionTwo description |
??D | ...                         |                         |
??D | [FunctionN](#FunctionN)     | FunctionN description   |
??D
??D <a id="FunctionOne"></a>
??D ### FunctionOne
??D
??D *typeReturn* **FunctionOne**( *type* **parameter** ) : *typeResult*
??D FunctionOne description.
??D
??D **Parameter**:
??D *type* **parameter** : Parameter description.
??D
??D **Result**:
??D *typeResult* : Result description.
??D
??D **Return**:
??D *typeReturn* **value** : Return description
??D
??D [Functions](#Functions) | [Index](#Index)
```

?? Change to:

| Doc Tag   | Description                                                                |
|----------:|:---------------------------------------------------------------------------|
| ##D       | Bash, Zsh, Python, Perl, Ruby                                              |
| //D       | C/C++, C#, Java, JavaScript, Pascal/Object Pascal, Go, Swift, Kotlin, Rust |
| --D       | SQL, Ada, Haskell                                                          |
| ''D       | Visual Basic, VBScript                                                     |
| %%D       | LaTex, MATLAB                                                              |

**Markdown Tags**:

| MD Tag        | Description                                 |
|--------------:|:--------------------------------------------|
| #            | Title, File Name                            |
| ##          | Title, Constants/Variables/Functions blocks |
| ###        | Title, Function Name                        |
| * *         | *Italic*, Types                             |
| ** **     | **Bold**, Function and Varible Names        |
| ''' ''' | Code Blocks                                 |
| ---          | Horizontal Line                             |

---
## Documentation Result

<a id="logFail"></a>
### functionName

*type return* **functionName**( *type* **param 1** , ... , *type* **param N** ) : *type result*
Function description.

**Parameter**:
*type* : Parameter description.

**Result**:
*type* : Result description.

**Return**:
*type* : Return description.

[Function](#Functions) | [Index](#Index)

---
<a id="Index"></a>
## Index

[Constants](#Constants)
[Variables](#Variables)
[Functions](#Functions)

---
<a id="Constants"></a>
## Constants

*integer*[] **Version** = (*1 0 0*)

[Index](#Index)

---
<a id="Variables"></a>
## Variables

*string* **Source** = *''*
*string* **Destine** = *''*

[Index](#Index)

---
<a id="Functions"></a>
## Functions

| Name                      | Description                                              |
|--------------------------:|:---------------------------------------------------------|
| [logFail](#logFail)       | Print a failure log message.                             |
| [unsetVars](#unsetVars)   | Unset global variables.                                  |
| [_exit](#_exit)           | End log, stop libShell, deinitialize variables and exit. |
| [_help](#_help)           | Print an help message.                                   |
| [parseArgs](#parseArgs)   | Parse parameters from command line.                      |
| [main](#main)             | Main shell script application.                           |

[Index](#Index)

---
<a id="logFail"></a>
### logFail

*none* **logFail**( *string* ) : *string*
Print a failure log message.

**Parameter**:
*string* : String log message.

**Result**:
*string* : Formatted log message.

**Return**:
*none*

[Function](#Functions) | [Index](#Index)

---
<a id="unsetVars"></a>
### unsetVars

*integer* **unsetVars**( *none* ) : *none*
Unset global variables.

**Parameter**:
*none*

**Result**:
*none*

**Return**:
*integer* : 0 Success

[Function](#Functions) | [Index](#Index)

---
<a id="_exit"></a>
### _exit

*integer* **_exit**( *none* ) : *none*
End log, stop libShell, deinitialize variables and exit an error code.

**Parameter**:
*none*

**Result**:
*none*

**Return**:
*integer* : 0 Success

[Function](#Functions) | [Index](#Index)

---
<a id="_help"></a>
### _help

*integer* **_help**( *none* ) : *string*
Print an help message.

**Parameter**:
*none*

**Result**:
*string* : Help message.

**Return**:
*integer* : 0 Success

[Function](#Functions) | [Index](#Index)

---
<a id="parseArgs"></a>
### parseArgs

*integer* **parseArgs**( *string*[] ) : *none*
Parse parameters from command line.

**-h**              Print help information about syntax and use.
[*file*]          Open file as input and save in a file with extension *.md

Options:
**-i** <*file*>       Generate documentation from input file.
**-o** <*file*>       Generate documentation into output file.
**--** [*parameters*] Send [parameters] to libShell.

**Result**:
*none*

**Return**:
*integer* : 0 Success
*integer* : 1 Failure

[Function](#Functions) | [Index](#Index)

---
<a id="main"></a>
### main

*integer* **main**( *string*[] ) : *none*
Main shell script application.

**Parameter**:
*string*[] : Parameter list from command line.

**Result**:
*none*

**Return**:
*integer* : 0    Success
*integer* : 1..N Failure

[Function](#Functions) | [Index](#Index)
