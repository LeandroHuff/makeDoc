#!/usr/bin/env python3
# coding=utf-8
##H>
##D><h1 id="top">makeDoc.py</h1>
import os
import re
import sys
import argparse

from modules import debug

##D><table id="description">
##D><tr><td align=right></td><td></td><td>Python program to export documentation from source code files.</td></tr>
##D><tr><td align=right><b>File</b></td><td>:</td><td>makeDoc.py</td></tr>
##D><tr><td align=right><b>Date</b></td><td>:</td><td>2025-09-30</td></tr>
##D><tr><td align=right><b>Author</b></td><td>:</td><td><a href="mailto:leandrohuff@email.com">Leandro</a></td></tr>
##D><tr><td align=right><b>Version</b></td><td>:</td><td>1.0.0 / 2025-09-30</td></tr>
##D><tr><td align=right><b>Copyright</b></td><td>:</td><td>CC01 1.0 Universal</td></tr>
##D></table>
##D><br>
##D><p>
##D><b>Note</b>: Changes in this documentation will be discarded on next build, any changes should be made on source code documentation instead. <br>
##D></p>


##D><br>
##D><h3 id="main">main( )</h3>
##D><i>integer</i> <b>main</b>( <i>string</i> <b>args</b> ) : <i>none</i> <br>
##D>&ensp;Main application program. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> <b>args</b> : Parameters from command line. <br>
##D>&ensp;<b>-h</b>, <b>--help</b>           show this help message and exit.  <br>
##D>&ensp;<b>-i</b>, <b>--input</b>  INPUT   Path and filename to the input file.  <br>
##D>&ensp;<b>-o</b>, <b>--output</b> OUTPUT  Path and filename to the output file.  <br>
##D>&ensp;<b>-f</b>, <b>--force</b>          Force yes for any question.  <br>
##D>&ensp;<b>-g</b>, <b>--debug</b>          Enable debug messages.  <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;<i>none</i> <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i>: <b>0</b> - Success <br>
##D>&ensp;<i>integer</i>: <b>1</b> - Failure <br>
def main(args) -> int:
    errorCode: int = 0
    inputFilename: str = ""
    outputFilename: str = ""
    flagSaveFooter: bool = False
    flagMakeDoc : bool = True

    parser = argparse.ArgumentParser(
        description="Read taged lines documentation from source code and export documentation to output file."
    )

    parser.add_argument(
        "-i", "--input", help="Path and filename to the input file."
    )
    parser.add_argument(
        "-o", "--output", help="Path and filename to the output file."
    )
    parser.add_argument(
        "-f", "--force", action="store_true", help="Force yes for any question."
    )
    parser.add_argument(
        "-g", "--debug", action="store_true", help="Enable debug messages."
    )

    args = parser.parse_args()

    if args.debug:
        debug.setDebugFlag(True)
        debug.log("Debug enabled.")

    if args.force:
        debug.log("Force mode is enabled.")

    if not args.input:
        debug.error(f"Empty parameter for input file.")
        errorCode = 1
        return errorCode
    elif not os.path.exists(args.input):
        debug.info(f"Input file {args.input} not found, no documentaion exported.")
        errorCode = 2
        return errorCode

    inputFilename = args.input
    debug.log(f"Input file: {inputFilename}")

    if not args.output:
        debug.error(f"Empty parameter for output file.")
        errorCode = 3
        return errorCode
    elif not args.force and os.path.exists(args.output):
        debug.warning(
            f"Output file {args.output} already exist, no documentation exported."
        )
        errorCode = 4
        return errorCode

    outputFilename = args.output
    debug.log(f"Output file: {outputFilename}")

    padraoRegex: str = r"^ *([#|']+|[/|'|%|-]{2,}[B|D|E])>? *"
    padraoBeginDoc: str = r"^ *([#|']+|[/|'|%|-]{2,}B>?) *"
    padraoDoc: str = r"^ *([#|']+|[/|'|%|-]{2,}D>?) *"
    padraoEndDoc: str = r"^ *([#|']+|[/|'|%|-]{2,}E>?) *"

    try:
        with (
            open(inputFilename, "r") as inputFile,
            open(outputFilename, "w") as outputFile,
        ):
            # make Documentation
            for linhaFile in inputFile:
                matchDoc = re.search(padraoRegex, linhaFile)
                if matchDoc:
                    if flagMakeDoc and re.search(padraoDoc, linhaFile):
                        textDoc = re.split(padraoDoc, linhaFile)
                        outputFile.write(textDoc[2])
                    elif re.search(padraoEndDoc, linhaFile):
                        flagMakeDoc = False
                        debug.info ("Make Doc disabled.")
                    elif re.search(padraoBeginDoc, linhaFile):
                        flagMakeDoc = True
                        debug.info ("Make Doc enabled.")
    except FileNotFoundError as err:
        errorCode = 3
        debug.log(f"File not found: {err}")
    except IOError as err:
        errorCode = 4
        debug.log(f"File I/O error: {err}")
    return errorCode


##D><br>
##D><h3 id="entry">Python Entry Point</h3>
##D>&ensp;Main application entry point. <br>
##D><br>
##D><b>Parameter</b>: <br>
##D>&ensp;<i>string</i> <b>args</b> : Parameters from command line. <br>
##D><br>
##D><b>Result</b>: <br>
##D>&ensp;Call main() function to run all application. <br>
##D>&ensp;Print a success or failure message. <br>
##D><br>
##D><b>Return</b>: <br>
##D>&ensp;<i>integer</i>: <b>0</b> - Success <br>
##D>&ensp;<i>integer</i>: <b>1</b> - Failure <br>
##F>
if __name__ == "__main__":
    ret: int = main(sys.argv)
    if ret == 0:
        debug.info("Export documentation.")
    else:
        debug.error("Export documentation.")
    exit(ret)
