'''
    @mainpage File to service function to send debug messages to terminal.

    @section Description
        This source code file has some functions to send messages to terminal for debug purpose.

    @file debug.py

    @brief File to service function to send debug messages to terminal.

    @section Author(s)
        - Created by Huff, Leandro D. on 05/27/2020.
        - Modified by Huff, Leandro D. on 05/19/2023.
'''

# Import module used to access file system.
import os

'''
  Import module used to get python version information,
  command line arguments, source code file name and line number.
'''
import sys

# Import module used to call external commands.
#from subprocess import call

# Include user modules
#from modules import  askuser
from modules import color
#from modules import  luks
#from modules import  console
#from modules import  codes

__all__ = [
    'setDebugFlag',
    'getDebugFlag',
    'log',
    'info',
    'error',
    'warning',
]

# Constants

# #  Enable/Disable an internal debug mode.
# ##   1 = enable
# ##   0 = disable
DEBUG = False
ASSERT = False

# Variables
Debug = DEBUG


def setDebugFlag(flag:bool=True) -> None:
    '''
    Function to set the Debug variable state.
    :param              bool            - Debug flag state.
    :return             none
    '''
    global Debug
    Debug = flag


def getDebugFlag() -> bool:
    '''
    Function to get the Debug variable state.
    :param              none
    :return             bool            - Debug flag state.
    '''
    global Debug
    return Debug


def function_name() -> str:
    '''
    Returns the source code functions name at function call.
    :param              none
    :return             str             - Source code function name at the position of function calling.
    '''
    return sys._getframe(2).f_code.co_name


def line_number() -> int:
    '''
    Returns the current source code line number at function call.
    :param:             none
    :returns            int             - Line number at the position of function calling.
    '''
    return sys._getframe(2).f_lineno


def file_name() -> str:
    '''
    Get the curret source code file name at position of function call.
    :param              none
    :return             str             - Source code file name at the position of function calling.
    '''
    return sys._getframe(2).f_code.co_filename


def log(msg:str, force:bool=False) -> None:
    '''
    Function to print debug messages on console.
    :param      msg     str             - Message to be sent to the terminal.
    :param      force   bool            - Force (True) print debug messages ignoring debug flag state.
    :return             none
    '''
    global Debug
    if not Debug and not force:
        return
    # function os.path.split( path+filename ) will split the string "path+filename" into 2 variables as:
    # path: receive the file path
    # fname: receive the filename.extension
    # Obs.: path variable is no longer used but it is needed by os.path.split() requirements the return 2 separated strings.
    path,fname = os.path.split( file_name() )
    print( '{}DEBUG:{}{} file:{}  line:{}  function:{}'.format( color.styleFg( color.NORMAL, color.LIGHT + color.GREEN ), color.reset(), msg, fname, str( line_number() ), function_name() ) )


def info(msg:str) -> None:
    '''
    Function to print info messages on console.
    :param      msg     str             - Message to be sent to the terminal.
    :return             none
    '''
    print('{}INFO:{} {}'.format(color.styleFg(color.NORMAL, color.LIGHT + color.GRAY), color.reset(), msg))


def error(msg:str) -> None:
    '''
    Print a error message to the terminal.
    :param      msg     str             - Message to be sent to the terminal.
    :return             none
    '''
    print('{}ERROR:{} {}'.format(color.styleFg(color.NORMAL, color.LIGHT + color.RED), color.reset(), msg))


def warning(msg:str) -> None:
    '''
    Print a warning message to the terminal.
    :param      msg     str             - Message to be sent to the terminal.
    :return             none
    '''
    print('{}WARNING:{} {}'.format(color.styleFg(color.NORMAL, color.LIGHT + color.CYAN), color.reset(), msg))
