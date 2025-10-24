!/usr/bin/env bash
Constants
Variables
Functions
get lib config version string.
get lib config version number.
get formatted lib config version string.
get formatted lib config string version.
get formatted lib config string date.
get formatted lib config ingeter date.
Get a tag name from parameter.
Get a value from parameter.
Save user configuration tag=vale to file.
Check filename parameters.
Get all tags
Get all values
Save header into filename.
E>
D> # User Configuration File
D>
D> |                 | Description |
D> | --------------: | :-----------|
D> |                 | User configuration file to store tags and values. |
D> | **File**        | _${filename}_ |
D> | **Date**        | _$(date '+%Y-%m-%d %H:%M')_ |
D> | **lib Version** | _${_version[0]}.${_version[1]}.${_version[2]}_ |
D> | **lib Date**    | _${_date[0]}-${_date[1]}-${_date[2]}_ |
D>
B>
Save comment + tag=value into filename.
Read tag list from user configuration file.
Read value list from user configuration file.
Read value list from user configuration file, update a default value list, and return its list.
Check filename parameters.
Get all tags
Get all values
search tag
do not compute empty lines
do not compute commented lines
take tag from line
