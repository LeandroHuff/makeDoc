################################################################################
#MD**Project**: Make Markdown Documentation from Source Code
#MD**File**   : makeDoc.sh
#MD**Version**: 1.0.0
#MD**Date**   : 2025.09.16
#MD**Author** : Leandro - leandrohuff@programmer.net
#MD**Brief**  : Extract MarkDown comment lines and write lines on document file,
#MDusual comment lines start with #, // and so on, for markdown, lines start with
#MDtag '#MD', is must important that the tag reside at first column in source code.
#MDAll characters after the tag will be written on document file.
#MDThe script program accept some parameters to controle source and destination
#MDfiles, also some behaviours and libShell setup.
#MDTags #MD without characters is just a newline in markdown document.
################################################################################

# This is a normal commented line and will not be written on documentation file.
#MD# Markdown Documentation from Source Code

#MD### Functions
#MD
#MD#### Follow link and get target file.
#MD*integer* **followLink**( *string* **link** ) : *string*
function followLink()
{
    echo 'followLink()'
    return 0
}
#MD
#MD#### Check parameter for block device.
#MD*boolean* **isBlockDevice**( *string* **device** ) : *none*
function isBlockDevice()
{
    echo 'isBlockDevice()'
    return 0
}

#MD
#MD### Variables
#MD
#MD*integer* **libTIMEOUT** = *10*
declare -i libTIMEOUT=10

#MD
#MD### Constants
#MD
#MD*string* **WHITE** = *'\033[97m'*
declare -r WHITE='\033[97m'
