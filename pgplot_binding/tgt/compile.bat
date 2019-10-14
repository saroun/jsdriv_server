@echo off
set tgt=%1
if [%tgt%]==[clean] (
	make -C ./ -f makefile_gfortran SRC=../src clean
	goto end
)
if [%tgt%]==[erase] (
	make -C ./ -f makefile_gfortran SRC=../src erase
	goto end
)

if [%tgt%]==[prog] (
	make -C ./ -f makefile_gfortran SRC=../src prog
	goto end
)

make -C ./ -f makefile_gfortran SRC=../src %tgt%
:end

