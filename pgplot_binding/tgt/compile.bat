@echo off
set tgt=%1
set opt="make -C ./ -f makefile_gfortran SRC=../src SYSDIR=../sys_mingw "

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

