@echo off

set MK=mingw32-make

rem get target
if %1.==. (
	goto help:
)
set tgt=%1
if "%tgt%"=="all" (goto ALL:)
if "%tgt%"=="prog" (goto ALL:)
if "%tgt%"=="clean" (goto CLR:)
if "%tgt%"=="erase" (goto CLR:)
if "%tgt%"=="install" (goto INST:)

:ALL
if %2.==. (
	goto help:
)
set src=%2
if EXIST %src% (
    echo SRC=%src% 
) ELSE (
	echo Source directory [%src%] does not exist
	goto end:
)
%MK% -f makefile_gfortran %tgt% SRC=%src%
goto end:

:CLR
%MK% -f makefile_gfortran %tgt% 
goto end:

:INST
if %2.==. (
	echo Missing parameter: target folder for binary files
	goto help:
)
set tdir=%2
if EXIST %tdir% (
    echo TGT=%tdir% 
) ELSE (
	echo Target directory [%tdir%] does not exist
	goto end:
)
if %3.==. (
	echo Missing parameter: folder for grfont.dat
	goto end:
)
set pgdir=%3
if EXIST %pgdir% (
    echo PGDIR=%pgdir% 
) ELSE (
	echo Target directory [%pgdir%] does not exist
	goto end:
)

%MK% -f makefile_gfortran %tgt% BIN=%tdir%  PGD=%pgdir%
goto end:

:help
echo Usage: 
echo %0 [all,prog] SRC
echo %0 [clean,erase] 
echo %0 install TGT PGD 
 
echo SRC = path to PGPLOT source distribution
echo TGT = folder for binary files (only for target=install)
echo PGD = folder for grfont.dat etc. (what should be PGPLOT_DIR variable) (only for target=install)
 
goto end:

:end

