
**Project**: Make Markdown Documentation from Source Code
**File**   : makeDoc.sh
**Version**: 1.0.0
**Date**   : 2025.09.16
**Author** : Leandro - leandrohuff@programmer.net
**Brief**  : Extract MarkDown comment lines and write lines on document file,
usual comment lines start with #, // and so on, for markdown, lines start with
tag '#MD', is must important that the tag reside at first column in source code.
All characters after the tag will be written on document file.
The script program accept some parameters to controle source and destination
files, also some behaviours and libShell setup.
Tags #MD without characters is just a newline in markdown document.
# Markdown Documentation from Source Code
### Functions

#### Follow link and get target file.
*integer* **followLink**( *string* **link** ) : *string*

#### Check parameter for block device.
*boolean* **isBlockDevice**( *string* **device** ) : *none*

### Variables

*integer* **libTIMEOUT** = *10*

### Constants

*string* **WHITE** = *'033[97m'*
