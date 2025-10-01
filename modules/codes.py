"""
  @mainpage This files declare some constants values to be used as codes.

  @section Description
    This file is available for all source code files in this project that use error codes,
    dafault values as strings or ints and constant values for any purposes.

  @section Notes
    - Encrypt format is Luks2.

  @file codes.py

  @brief This files declare some constants values to be used as codes to:
    - Error codes.
    - Constants values.
    - Defaults values.

  @section Author(s)
    - Created by Huff, Leandro D. on 05/27/2020.
    - Modified by Huff, Leandro D. on 05/19/2023.
"""

"""
  Import module OS used to access file system.
"""
#import os

"""
  Import module SYS used to get python version information,
  command line arguments, source code file name and line number.
"""
#import sys

""" Import module SUBPROCCESS used to call external commands. """
#from subprocess import call

""" Include user modules """
#from modules import askuser
#from modules import debug
#from modules import color
#from modules import luks
#from modules import console

__all__ = [
  'SUCCESS',
  'FAILURE',
  'MOUNT_ERR',
  'UMOUNT_ERR',
  'NOT_DIR',
  'DIR_ALREADY_EXIST',
  'DIR_NOT_FOUND',
  'CREATE_DIR_ERR',
  'REMOVE_DIR_ERR',
  'DIR_WR_ACCESS_ERR',
  'DIR_RD_ACCESS_ERR',
  'DIR_RW_ERR',
  'NOT_FILE',
  'FILE_ALREADY_EXIST',
  'FILE_NOT_FOUND',
  'FILE_WR_ACCESS_ERR',
  'FILE_RD_ACCESS_ERR',
  'FILE_RW_ERR',
  'NOT_BLOCK_DEVICE',
  'BLOCK_DEVICE_ALREADY_EXIST',
  'BLOCK_DEVICE_NOT_FOUND',
  'NOT_LUKS_DRIVE',
  'IS_LUKS_DRIVE',
  'STR_NULL',
  'FILE_FORMAT',
  'ROOT',
]

# Generic codes.
FAILURE = -1
SUCCESS = 0

# Mount/Umount error codes.
MOUNT_ERR = 10
UMOUNT_ERR = 11

# Directories error codes.
NOT_DIR = 20
DIR_ALREADY_EXIST = 21
DIR_NOT_FOUND = 22
CREATE_DIR_ERR = 23
REMOVE_DIR_ERR = 24
DIR_WR_ACCESS_ERR = 25
DIR_RD_ACCESS_ERR = 26
DIR_RW_ERR = 27

# File error codes.
NOT_FILE = 30
FILE_ALREADY_EXIST = 31
FILE_NOT_FOUND = 32
FILE_WR_ACCESS_ERR = 33
FILE_RD_ACCESS_ERR = 34
FILE_RW_ERR = 35

# Block device error codes.
NOT_BLOCK_DEVICE = 40
BLOCK_DEVICE_ALREADY_EXIST = 41
BLOCK_DEVICE_NOT_FOUND = 42

# Luks drive error codes.
NOT_LUKS_DRIVE = 50
IS_LUKS_DRIVE = 51

# Strings
STR_NULL = ''
FILE_FORMAT = 'ext4'

# Users
ROOT = True
