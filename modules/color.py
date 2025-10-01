"""
    @mainpage Python Script to Encrypt File and External Drives to Luks Format.

    @section Description
        Python script routines to encrypt files as encrypted devices and encrypt
        external drives to be accessed as a standard device, all data will be
        encrypted to Luks2 format.

    @section Notes
        - Encrypt format is Luks2.

    @file color.py

    @brief This module service functions for escape codes and text color attributes to
            print texts with background and foreground colors on console and terminal.

    @section Author(s)

    - Created by Huff, Leandro D. on 05/27/2020.
    - Modified by Huff, Leandro D. on 05/19/2023.
"""

__all__ = [
    'styleFrBg',
    'styleFg',
    'reset',
    'NORMAL',
    'BOLD',
    'ITALIC',
    'UNDERLINE',
    'DARK',
    'HIGH',
    'BLACK',
    'RED',
    'GREEN',
    'YELLOW',
    'BLUE',
    'MAGENTA',
    'CYAN',
    'GRAY',
    'FG',
    'BG',
]

# # Escape codes for terminal text styles.
# ##   0 = normal text.
# ##   1 = bold text.
# ##   3 = italic text.
# ##   4 = underline text.
NORMAL = 0
BOLD = 1
ITALIC = 3
UNDERLINE = 4

# # Escape code for highlight text color.
# ##   60 = highlight foreground/background text color.
DARK= 0
LIGHT = 60

# # Base text colors, can be used as foreground and background and combined with highlights text color.
# ##   0 = black
# ##   1 = red
# ##   2 = green
# ##   3 = yellow
# ##   4 = blue
# ##   5 = magenta/
# ##   6 = cyan/high cyan
# ##   7 = gray/white
BLACK = 0
RED = 1
GREEN = 2
YELLOW = 3
BLUE = 4
MAGENTA = 5
CYAN = 6
GRAY = 7

# # Foreground escape codes.
FG = 30
# # Background escape codes.
BG = 40


def styleFrBg(st:int, fg:int, bg:int) -> str:
    """
    Function to set the text color for style, foreground and background.
    :param st: Set the style for the text, can be 0, 1, 3, 4 for normal, bold, italic and underlined respectively.
    :param fg: Set foreground text color that will be added to 30.
    :param bg: Set background text color that will be added to 40.
    :return: A formated string text to be used on print function.
    """
    return "\x1b[{};{};{}m".format(st, (FG + fg), (BG + bg))


def styleFg(st:int, fg:int) -> str:
    """
    Function to set the text color for style, foreground.
    :param st:  Set the style for the text, can be 0, 1, 3, 4 for normal, bold, italic and underlined respectively.
    :param fg:  Set foreground text color that will be added to 30.
    :return:    A formated string text to be used on print function.
    """
    return "\x1b[{};{}m".format(st, (FG + fg))


def reset() -> str:
    """
    Function to reset the text color.
    :return: A formated string text to be used on reset text colors.
    """
    return '\x1b[0m'
