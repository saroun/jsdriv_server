# jsdriv_server
PGPLOT graphics device for Windows

Written by: J.Saroun, saroun@ujf.cas.cz

## Features

- Allows to use PGPLOT interactive graphics on Windows 
- Requires mingw-w64 gcc compiler suite to compile 
- Developed and tested on Windows 10, 64 bit
- Provides a new JSDRIV device server (analogy to pgxwin_server on Linux)

=====================================================================

## Package contents

### ./jsdriv
Lazarus project files which allow to build:

`jsdrivlib.dll`, a library dynamically loaded by `libpgplot.dll` (linked with client applications)

`jsdriv_server.exe `, a server providing persistent graphics windows (similar to pgxwin_server on Linux) 

### ./pgplot_binding 
Extensions to PGPLOT needed to create libpgplot.dll which supports the JSDRIV device. 
	
`sys_mingw` - Source files to be be merged with the original PGPLOT source distribution

`tgt` - Target directory for compilation, including:

`makeflie_gfortran` - makefile for compiling PGPLOT using mingw-w64 gcc + gfortran (includes drivers: NULL, PSDRIV, JSDRIV)	

`grexec.f` - Generated driver function GREXEC for the NULL, PSDRIV and JSDRIV devices.

`compile.bat` - A batch file for compilig PGPLOT. Call from command line:
	

## Compiling JSDRIV server and client library

- Install Lazarus FreePascal RAD (https://www.lazarus-ide.org/) if you don't have it already
- Build the project `jsdrivlib.lpr` to create the client library, `jsdrivlib.dll`.
- Build the project `jsdriv_server.lpr` to create the server, `jsdriv_server.exe`

## Compiling PGPLOT with JSDRIV support:

(paths are relative to the `./pgplot_binding` folder)
- Download the original PGPLOT ver. 5.2 source files from http://www.astro.caltech.edu/~tjp/pgplot/)  
- Create a directory `./src` and extract PGPLOT sources to it
- Open Windows command line and make `./tgt` your current directory
- Make sure you call the correct gcc compiler from mingw-w64 distribution, try `> gcc --version`
- Run `compile.bat` to build libpgplot.dll
- Run `compile.bat prog` to build PGPLOT examples
- Run `compile.bat clean` to remove temporary object files


## Running PGPLOT examples

- Copy jsdriv_server.exe and jsdrivlib.dll to ./tgt
- From Windows command line, make ./tgt your current directory
- Define environment variables:
  PGPLOT_DEV=/JSDRIV
  PGPLOT_DIR="full path to ./tgt"
- Run the pgdemo*xx*.exe examples from the command line

## Distribution + installation notes

- Install libpgplot.dll and jsdrivlib.dll alongside with your application executable, 
   or in the system directory: %SystemRoot%\system32
- On systems without mingw-w64 installed, some other mingw runtime libraries may be needed:
	- libquadmath-0.dll
	- libgcc_s_seh-1.dll
- Install grfont.dat and jsdriv_server.exe in the directory defined in the PGPLOT_DIR variable. 
  Recommended place is the folder with the dll's and the application binaries  
  
