An extension to the PGPLOT library for Windows
Written by: J.Saroun, saroun@ujf.cas.cz
Licence: see LICENSE.txt

- allows to use PGPLOT with interactive graphics on Windows 
- requires mingw-w64 gcc compiler suite to compile 
- developed and tested on Windows 10, 64 bit
- uses a new JSDRIV device server (analogy to pgxwin_server on linux)
=====================================================================

1. Package contents:
=====================

./src
	PGPLOT source distribution directory 
	(target for unpacking pgplot5.2.tar.gz from http://www.astro.caltech.edu/~tjp/pgplot/)
	
./sys_mingw
	PGPLOT files to be be merged with the original PGPLOT source files in ./src
	
./tgt
	Target directory for compilation
	
./tgt/makeflie_gfortran
	makefile for compiling PGPLOT using mingw-w64 gcc + gfortran
	(includes drivers: NULL, PSDRIV, JSDRIV)	
	
./tgt/grexec.f
	Generated driver function GREXEC for the NULL, PSDRIV and JSDRIV devices.

./tgt/compile.bat
	A batch file for compilig PGPLOT. Call from command line:
	> compile       # compile libpgplot.dll and its import library: libpgplot.lib
	> compile prog  # compile demo examples
	> compile clean # remove all object files
	> compile erase # remove all object files + all executables
	
./jsdriv
	Source files for compiling jsdrivlib.dll and jsdriv_server.exe (with Lazarus project files) 

2. Compiling PGPLOT:
====================
- From Windows command line, make ./tgt your current directory
- Make sure you call the correct gcc compiler from mingw-w64 distribution
  > gcc --version
- Get the original PGPLOT ver. 5.2 source files at http://www.astro.caltech.edu/~tjp/pgplot/)  
- Extract the package to ./src
- Copy the folder ./sys_mingw to ./src
- Run "compile.bat:" to build libpgplot.dll
- Run "compile.bat prog" to build PGPLOT examples

3. Compiling JSDRIV server and client library
============================================
- Install Lazarus FreePascal RAD (https://www.lazarus-ide.org/) if you don't have it already
- Build the project "jsdrivlib.lpr" to create the client library, jsdrivlib.dll.
- Build the project "jsdriv_server.lpr" to create the server, jsdriv_server.exe

4. Running PGPLOT examples
===========================
- Copy jsdriv_server.exe and jsdrivlib.dll to ./tgt
- From Windows command line, make ./tgt your current directory
- Define environment variables:
  PGPLOT_DEV=/JSDRIV
  PGPLOT_DIR="full path to ./tgt"
- Run the pgdemoxx.exe examples from the command line

5. Distribution + installation notes
=====================================
- Install libpgplot.dll and jsdrivlib.dll alongside with your application executable, 
   or in the system directory: %SystemRoot%\system32
- On systems without mingw-w64 installed, some other mingw runtime libraries may be needed:
	- libquadmath-0.dll
	- libgcc_s_seh-1.dll
- Install grfont.dat and jsdriv_server.exe in the directory defined in the PGPLOT_DIR variable. 
  Recommended place is the folder the dll's and the application binaries  
  
  