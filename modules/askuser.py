"""
  @mainpage This files declare some function to read input user data values from terminal.

  @section Description
    This file is available for all source code files in this project that use inputs to
    read data from terminal.

  @section Notes
    - Read data from terminal.

  @file askuser.py

  @brief This files declare some function to read input user data values from terminal.

  @section Author(s)
    - Created by Huff, Leandro D. on 05/27/2020.
    - Modified by Huff, Leandro D. on 05/19/2023.
"""

""" Import module used to access file system. """
#import os

"""
  Import module used to get python version information,
  command line arguments, source code file name and line number.
"""
#import sys

"""
"""
import shlex

""" Import module used to call external commands. """
#from subprocess import call

""" Include user modules """
from modules import debug
#from modules import color
#from modules import luks
from modules import console
from modules import codes

""" Share this functions with other modues. """
__all__ = [
    'forDrive',
    'forDevice',
    'forPath',
    'forFile',
    'forSize',
    'forFormat',
]

""" File system formats """
fileFormats = ('', 'ext2', 'ext3', 'ext4', 'btrfs', 'exfat', 'fat', 'ntfs')


def forDrive() -> str:
    """
    Function to ask the user to input a drive name.
    :param      none
    :return     string      drive       - string name of extern drive at /dev/*.
    """
    cmd = 'ls /dev/sd*'
    console.runCommand(cmd, 10.0)
    answer = input('Enter a drive name at ' + console.DRIVE_DIR + '/')
    sdx = shlex.split(answer)
    return sdx[0].__str__()


def forDevice() -> str:
    """
    Function to ask the user to input a device name.
    :param      none
    :return     string      device      - string name of internal device at /mnt/*.
    """
    answer = input('Enter a device\'s name at ' + console.MOUNT_DIR + '/')
    dvc = shlex.split(answer)

    return dvc[0].__str__()


def forPath() -> str:
    """
    Function to read a path to be used with file name.
    :param      none
    :return     string                  - File path.
    """
    answer = input('Enter a file path: ')
    fpath = shlex.split(answer)
    return fpath[0].__str__()


def forFile() -> str:
    """
    Function to read a file name to be used as an encrypted device.
    :param      none
    :return     string                  - File name.
    """
    answer = input('Enter a file /path/../name: ')
    fname = shlex.split(answer)
    return fname[0].__str__()


def forSize() -> int:
    """
    Function to read a file size in MB (megabytes).
    :param      none
    :return     int                     - File size in MB.
    """
    fsize:int = 0

    while fsize <= console.FILE_SZ_MB:
        answer = input('Type a file size number in MB greater or equal to ' + str(console.FILE_SZ_MB) + ', click [ENTER] for ' + str(console.FILE_SZ_MB) + 'MB as default: ')
        resp = shlex.split(answer)
        if resp[0] == '':
            fsize = console.FILE_SZ_MB
        else:
            try:
                fsize = int(resp[0])
            except:
                debug.error('Input ( ' + resp[0] + ' ) is not a valid number.')
                fsize = 0

    return fsize


def forFormat(device:str) -> str:
    """
    Function to ask the user to input a file format.
    :param      string      device      - Device for file format.
    :return     string      format      - File system format.
    """
    resp = 'not valid'

    while resp not in fileFormats:
        answer = input ('Type ( ' + fileFormats.__str__() + ' ), [ENTER] for \"' + codes.FILE_FORMAT + '\" as default: ')
        resp = shlex.split(answer)

    if resp[0] != '':
        return resp[0]

    return codes.FILE_FORMAT
