# jsdriv_server
PGPLOT graphics device for Windows

Written by: Jan Saroun, saroun@ujf.cas.cz

## Features

- Allows to use PGPLOT interactive graphics on Windows 
- Provides a new JSDRIV device server (analogy to pgxwin_server on Linux)
- Requires mingw-w64 gcc compiler suite to compile PGPLOT with JSDRIV support
- Requires Lazarus FreePascal to compile the graphics device library 
- Developed and tested on Windows 10, 64 bit

=====================================================================

## Package contents

### ./jsdriv
Lazarus project files (`*.lpr`) which allow to build:

`jsdrivlib.dll`, a library dynamically loaded by `libpgplot.dll` (to be linked with client applications)

`jsdriv_server.exe `, a server providing persistent graphics windows (similar to pgxwin_server on Linux) 

### ./pgplot_binding 
Extensions to PGPLOT needed to create `libpgplot.dll` which supports the JSDRIV device. 
	
`sys_mingw` - Source files to be compiled with the original PGPLOT source distribution

`makeflie_gfortran` - makefile for compiling PGPLOT using mingw-w64 gcc + gfortran (includes drivers: NULL, PSDRIV, JSDRIV)	

`grexec.f` - Generated driver function GREXEC for the NULL, PSDRIV and JSDRIV devices.

`compile.bat` - A batch file for compilig PGPLOT with JSDRIV support
	

## Compiling JSDRIV server and client library

- Install Lazarus FreePascal RAD (https://www.lazarus-ide.org/) if you don't have it already
- Build the project `jsdrivlib.lpr` to create the client library, `jsdrivlib.dll`.
- Build the project `jsdriv_server.lpr` to create the server, `jsdriv_server.exe`

The binaries are saved in ./jsdriv/bin, you should move them to appropriate directories:
- copy `jsdrivlib.dll` alongside with the program executable  
- copy `jsdriv_server.exe` alongside with `grfont.dat` (i.e. where the environment variable `PGPLOT_DIR` points to) 

## Compiling PGPLOT with JSDRIV support:

(paths are relative to the `./pgplot_binding` folder)
- Download the original PGPLOT ver. 5.2 source files from http://www.astro.caltech.edu/~tjp/pgplot/)  
- Extract it to a new directory, e.g.  `./src`
- Open Windows command line and make `./pgplot_binding` your current directory
- Make sure you call the correct gcc compiler from mingw-w64 distribution, try `> gcc --version`
- Run `compile all ./src` to build libpgplot.dll
- Run `compile prog ./src` to build PGPLOT examples
- Run `compile clean` to remove temporary object files

See the contents of `compile.bat` to learn how to use `makeflie_gfortran` directly.

## Test - run PGPLOT examples

- Copy `jsdriv_server.exe` and `jsdrivlib.dll` to `./pgplot_binding`
- From Windows command line, make ./tgt your current directory
- Define environment variables:
  PGPLOT_DEV=/JSDRIV
  PGPLOT_DIR="full path to `./pgplot_binding`"
- Run the pgdemo*xx*.exe examples from the command line

*NOTE: when running te program for the first time, an anitvirus program may slow down the startup of jsdriv_server. Just wait for ~ 10 sec till the server starts. Its icon should be visible on the tray.*

## Distribution + installation notes

- Install `libpgplot.dll` and `jsdrivlib.dll` alongside with your application executable, 
   or in the system directory: `%SystemRoot%\system32`
- On systems without mingw-w64 installed, some other mingw runtime libraries may be needed:
	- `libquadmath-0.dll`
	- `libgcc_s_seh-1.dll`
- Install `grfont.dat` and `jsdriv_server.exe` in the directory defined in the PGPLOT_DIR variable. Recommended place is the folder with the dll's and the application binaries.
  
