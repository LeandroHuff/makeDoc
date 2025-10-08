#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /var/home/leandro/dev/makeDoc/lazarus/_makeDoc
OFS=$IFS
IFS="
"
/usr/sbin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2     -L. -o /var/home/leandro/dev/makeDoc/lazarus/_makeDoc -T /var/home/leandro/dev/makeDoc/lazarus/link12965.res -e _start
if [ $? != 0 ]; then DoExitLink /var/home/leandro/dev/makeDoc/lazarus/_makeDoc; fi
IFS=$OFS
