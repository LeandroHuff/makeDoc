"""
    @mainpage File to service function to treat input and output on terminal and file system.

    @section Description
        This source code service some functions to treat input and output messages, data and
        files in the filesystem.

    @section Notes
        - Functions to service input and output on terminal.

    @file console.py

    @brief File to service function to treat input and output on terminal and file system.

    @section Author(s)
        - Created by Huff, Leandro D. on 05/27/2020.
        - Modified by Huff, Leandro D. on 05/19/2023.
        - Modified by Huff, Leandro D. on 12/01/2023.
            - Changes on MESSAGE_CHK text for better user advice.
            - Changes on checkDeviceWriteAccess function to check directory access error.
"""

""" Import module used to access file system. """
import os
import stat

"""
  Import module used to get python version information,
  command line arguments, source code file name and line number.
"""
#import sys

""" Import module used to call external commands. """
#import shlex
import subprocess
from subprocess import CalledProcessError, TimeoutExpired, CompletedProcess, STDOUT

""" Include user modules """
#from modules import askuser
from modules import debug
#from modules import color
#from modules import luks
from modules import codes

__all__ = [
    'runCommand',
    'itExist',
    'isDir',
    'isBlockDevice',
    'getLinkPointTo',
    'isLinkToBlockDevice',
    'isDevice',
    'isLink',
    'isBlock',
    'makeDir',
    'checkDeviceWriteAccess',
    'checkDeviceReadAccess',
    'allocFile',
    'formatDevice',
    'mountDevice',
    'chownDevice',
    'removeMediaDir',
    'umountDevice',

    'user',
    'root',

    'FOLDER_CHK',
    'FILE_CHK',
    'MESSAGE_CHK',
    'DRIVE_DIR',
    'MOUNT_DIR',
    'MAPPER_DIR',
    'FILE_SZ_MB',
    'BLOCK_SZ',
]

# Console commands
CMD_SUDO = 'sudo '
CMD_MKDIR = 'mkdir '
CMD_TOUCH = 'touch '
CMD_RM_DIR = 'rm -d '
CMD_RM_FILE = 'rm -i '

# # Directory path for drive, device and file access, file verification path and name, message confirmation.'
# ##     DRIVE_DIR   Base path for external drive.
# ##     MOUNT_DIR   Base path for internal device mount.
# ##     MAPPER_DIR  Base path for a mapped device's name.
# ##     FILE_CHK    File check name used to verify the running device.
# ##     MESSAGE_CHK Message used to save into the file check.
# ##     FILE_SZ_MB  Internal file size in megabytes (MB) to store an internal encrypted device.
# ##     BLOCK_SZ    Internal block size in bytes (B) to alloc space for an external device or an internal file.
FOLDER_CHK = '/verify'
FILE_CHK = 'README.md'
MESSAGE_CHK = 'Encrypted device by Luks engine has been opened successfully.\n'
DRIVE_DIR = '/dev'
MOUNT_DIR = '/mnt'
MAPPER_DIR = '/dev/mapper'
FILE_SZ_MB = 512
BLOCK_SZ = 512
TOUT_DEFAULT = (60)  # segundos


def runCommand(cmd:str, tmout:float=TOUT_DEFAULT) -> int:
    """
    Function to run an external shell command and return the results.
    :param      cmd         str         - String command to be running.
    :param      tmout       float       - Timeout value in seconds to wait for command finish.
    :return                 int         - 0 for success, otherwise is an error code.
    """
    debug.log(cmd)
    try:
        ret = subprocess.call(cmd, shell=True, text=True, encoding='UTF-8', timeout=tmout)
    except CalledProcessError as err:
        debug.log('Error on call process to run command.')
    except TimeoutExpired as err:
        debug.log('Timeout on call process to run command.')
    else:
        debug.log("Return: " + ret.__str__())
    return ret


def isFile(file:str) -> bool:
    """
    Function to check if a path is a regular file in the file system.
    :param      str         file        - File path and name.
    :return     bool                    - True:  Is a file.
                                        - False: Is not a file.
    """
    return os.path.isfile(file)


def exist(path:str) -> bool:
    """
    Function to check if a file and path exist in the file system.
    :param      path        str         - File path and name.
    :return                 bool        - True:  Found the file in file system.
                                        - False: File or device not found.
    """
    return os.path.exists(path)


def isDir(dir:str) -> bool:
    """
    Function to check if dir exist.
    :param      dir         str         - Directory path.
    :return                 bool        - True: Directory exist in the file system.
                                        - False: It doesn't exist in the file system.
    """
    return os.path.isdir(dir)


def isBlockDevice(path:str) -> int:
    """
    Function to check if a path is a block device.
    :param      path        str         - Device path to be checked.
    :return                 int         - SUCCESS:          Is a block device.
                                        - DIR_NOT_FOUND:    Path not found.
                                        - NOT_BLOCK_DEVICE: Path is not a block device.
    """
    try:
        st = os.stat(path)
    except (OSError, ValueError):
        return codes.DIR_NOT_FOUND
    if not stat.S_ISBLK(st.st_mode):
        return codes.NOT_BLOCK_DEVICE
    return codes.SUCCESS


def getLinkPointTo(link:str) -> str:
    """
    Function to get the real path of a symbolic link.
    :param      link        str         - Link path and name.
    :return                 str         - The real path of a symbolic link.
    """
    try:
        st = os.lstat(link)
    except (OSError, ValueError):
        return ''
    link_point_to = os.path.realpath(link)
    return link_point_to


def isLinkToBlockDevice(link:str) -> int:
    """
    Function to check if symbolic link point to a block device descriptor.
    :param      link        str         - Symbolic link.
    :return                 int         - SUCCESS: The link point to a device block descriptor.
                                        - DIR_NOT_FOUND: Link path and name not found.
                                        - NOT_BLOCK_DEVICE: The symbolic link does not point to a device block descriptor.
    """
    try:
        st = os.lstat(link)
    except (OSError, ValueError):
        return codes.DIR_NOT_FOUND
    parent = os.path.realpath(link)
    return isBlockDevice(parent)


def isLink(link:str) -> bool:
    """
    Function to check if a symbolic link exist in the file system.
    :param      link        str         - Device path.
    :return                 int         - 0: Not exist.
                                        - 1: Exist.
    """
    return os.path.islink(link)


def makeDir(directory:str, sudo=False) -> int:
    """
    Function to create a directory.
    :param      ddirectory  str         - Directory/folder path to be created.
    :param      sudo        bool        - Flag to create a directory as a sudo user.
    :return                 int         - 0: Success
                                        - 1: Failure
    """
    if isDir(directory):
        return codes.DIR_ALREADY_EXIST
    cmd = ''
    if sudo: cmd += CMD_SUDO
    cmd += CMD_MKDIR + directory
    return runCommand(cmd)


def checkDeviceWriteAccess(path:str, file:str) -> int:
    """
    Function to verify a device access.
    :param      path        str         - Path to file.
    :param      file        str         - File name.
    :return                 int         - 0 for success, otherwise an error code.
    """
    ret = makeDir(path, False)
    if ret != codes.SUCCESS and ret != codes.DIR_ALREADY_EXIST:
        debug.log('Error: Can not create or access ' + path)
        return codes.DIR_WR_ACCESS_ERR

    if os.access(path, os.W_OK):
        try:
            fd = open(file, mode='w')
            fd.write(MESSAGE_CHK)
            fd.flush()
            fd.close()
        except PermissionError:
            debug.log('Error: Open ' + path + ' write access.')
            return codes.FILE_WR_ACCESS_ERR
    else:
        debug.log('Error: Dir ' + path + ' write access.')
        return codes.DIR_WR_ACCESS_ERR

    return codes.SUCCESS


def checkDeviceReadAccess(device:str, file:str) -> int:
    """
    Function to check read access to a device.
    :param      device      str         - Devices name to be verified.
    :return                 int         - 0: Success
                                        - 1: Failure
    """
    if os.path.isfile(file):
        try:
            fd = open(file, mode='r')
            line = fd.read()
            fd.close()
            debug.info(line)
        except PermissionError:
            debug.log('Error: Open ' + file + ' read access.')
            return codes.FILE_RD_ACCESS_ERR
    else:
        if checkDeviceWriteAccess(device, file) != codes.SUCCESS:
            debug.log('Error: Device ' + file + ' write access.')
            return codes.DIR_WR_ACCESS_ERR

    return codes.SUCCESS


def allocFileSpace(file:str, nblocks:int, blocksz:int=BLOCK_SZ, sudo:bool=False) -> int:
    """
    Function to alloc n contiguous blocks of blocksz size for a file, the total size is (blocksz * nblocks).
    :param      file        str         - File path and name.
    :param      nblocks     int         - Num of blocks of blocksz size.
    :param      blocksz     int         - Block size in bytes.
    :param      sudo        bool        - Flag to identify to run as sudo user.
    :return                 int         - 0 for success, otherwise an error code.
    """
    cmd = ''
    if sudo:
        cmd = CMD_SUDO

    if not exist(file):
        cmd += CMD_TOUCH + file
        ret = runCommand(cmd)
        if ret != codes.SUCCESS:
            debug.log('Error: Create ' + file)
            return ret

    cmd = ''
    if sudo:
        cmd = CMD_SUDO

    cmd += 'dd if=/dev/zero of=' + file + ' bs=' + str(blocksz) + ' count=' + str(nblocks) + ' status=progress'
    ret = runCommand(cmd)

    if ret != codes.SUCCESS:
        debug.log('Error: File allocation blocks.')

    return ret


def formatDevice(device:str, frmt:str) -> int:
    """
    Function to format a device.
    :param      device      str         - Device path and name to be formatted.
    :param      frmt        str         - Partition table format.
    :return                 int         - SUCCESS: Device was formatted successfully.
                                        - FAILURE: An error happening while format the device.
    """
    mapper = os.path.join(MAPPER_DIR, device)
    cmd = 'sudo mkfs.' + frmt + ' ' + mapper
    ret = runCommand(cmd)

    if ret != codes.SUCCESS:
        debug.log('Error: ' + cmd)
        return codes.FAILURE

    return codes.SUCCESS


def mountDevice(drive:str, device:str) -> int:
    """
    Function to mount a device under /media/ directory.
    :param      drive                   - Drive path and name.
    :param      device      str         - Device path and name.
    :return:                int         - SUCCESS: Everythings run fine.
                                        - DIR_WR_ACCESS_ERR: Access error to create a directory.
                                        - MOUNT_ERR: Error while try to mount device at /mnt dir.
    """
    mapper = os.path.join(MAPPER_DIR, device)
    media = os.path.join(MOUNT_DIR, device)
    ret = makeDir(media, True)  # True mean root parameter

    if ret != codes.SUCCESS:
        debug.log('Error: Create dir ' + media + ' failure.')
        if luks.Close(device) != codes.SUCCESS:
            debug.log('Error: Close luks device failure.')
        return codes.DIR_WR_ACCESS_ERR

    cmd = 'sudo mount ' + mapper + ' ' + media
    ret = runCommand(cmd)

    if ret != codes.SUCCESS:
        debug.error(cmd)
        return codes.MOUNT_ERR

    return codes.SUCCESS


def chownDevice(device:str) -> int:
    """
    Function to change device user and group rigths under media directory.
    :param      media       str         - Dir path to be changed.
    :return                 int         - SUCESS: The user rights was accepted.
                                        - FAILURE: The user rights was not accepted.
    """
    media = os.path.join(MOUNT_DIR, device)
    cmd = 'sudo chown -R $USER:$GROUP ' + media
    ret = runCommand(cmd)

    if ret != codes.SUCCESS:
        debug.log('Error: ' + cmd)
        return codes.FAILURE

    return codes.SUCCESS


def removeMediaDir(device:str) -> int:
    """
    Function to remove a device dir (media) from MOUNT_DIR (/mnt/).
    :param      device      str         - Device name to be removed.
    :return                 int         - SUCCESS: The directory at /mnt was removed.
                                        - DIR_NOT_FOUND: The directory does not exist already.
    """
    media = os.path.join(MOUNT_DIR, device)
    debug.log(media)

    if not os.path.exists(media):
        debug.log('Error: ' + media + ' does not exist.')
        return codes.DIR_NOT_FOUND

    cmd = 'sudo rm -d ' + media
    ret = runCommand(cmd)

    if ret != codes.SUCCESS:
        debug.log('Error: ' + cmd)

    return ret


def umountDevice(device:str) -> int:
    """
    Function to umount a device directory.
    :param      device      str         - Device path and name to be umounted.
    :return                 int         - SUCCESS: Unmount device successfully.
                                        - DIR_NOT_FOUND: Device does not exist.
                                        - FAILURE: An error happening.
    """
    media = os.path.join(MOUNT_DIR, device)
    debug.log(media)

    if not exist(media):
        debug.log('Error: dir ' + media + ' does not exist anymore.')
        return codes.DIR_NOT_FOUND

    if not isBlockDevice(media):
        debug.log('Error: ' + device + ' is not flagged as a block device.')

    cmd = 'sudo umount -l ' + media
    ret = runCommand(cmd)

    return ret
