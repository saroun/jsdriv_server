@echo off

if %1.==. (
	echo "Provide 1st parameter: make target [default|prog|clean|erase]"
	goto end:
)

if %2.==. (
	echo "Provide 2nd parameter: *full* path to PGPLOT source distribution"
	goto end:
)

set tgt=%1
set src=%2
if EXIST %src% (
    echo SRC=%src% 
) ELSE (
	echo "Source directory [%src%] does not exist"
	goto end:
)

set opt="make -C ./ -f makefile_gfortran SRC=%src% SYSDIR=./sys_mingw "

if [%tgt%]==[clean] (
	make %opt% clean
	goto end
)
if [%tgt%]==[erase] (
	make %opt%  erase
	goto end
)

if [%tgt%]==[prog] (
	make %opt%  prog 
	goto end
)

make %opt%  %tgt%

:end

