## Theriak-Domino Repository
This repository contains the development version of the 
Theriak-Domino source code in the main branch of the repository. 
Various source code releases appear under different 
tags. The versioning scheme follows the traditional Theriak-Domino
versioning by date: vYYYY.MM.DD.

## Download and Build

```
git clone https://github.com/Theriak-Domino/theriak-domino.git .

git tag -l
```

Choose the tag release of your preference, for example v2022-12-14:

```
git checkout v2022-12-14
cd src
```

To compile on mac, for example:

```
make -fMakefileMAC all
```

Copy the produced binaries to your install location. See the manual
in dist/docs directory for installation instructions.

## Notes For Building From Source

Git clone the repository. Alternatively, choose your brach or
tag of interest at the top left, and download the source using
the Code button at the top right. The src directory  contains
several makefiles to choose from depending on your OS and compiler. 
Known OS and compiler compatibility are shown in the table below.


### Building on MacOS, Apple Silicon Processor

For macs equipped with an Apple Silicon processor, the makefile 
provided in the src directory requires gfortran to compile and
build the suite of programs (see the next section if your mac 
is equipped with an Intel processor). If you do not have gfortran
installed, I recommend installing using HomeBrew. From the 
Terminal window, navigate to the src directory and build all 
targets:

```
make -fMakefileMAC all
```

This will build all binaries and place them in the 'build' 
directory, alongside the src directory. This directory will 
automatically be created when it does not exist. If you receive 
an error regarding modules, run the clean target first to 
remove all pre-existing *.mod files, if any:

```
make -fMakefileMAC clean
make -fMakefileMAC all
```

### Building on MacOS, Intel Processor

Both the gfortran compiler and the Intel ifort compiler 
can compile and build the suite of programs on Intel macs.
You will first need to edit the top portion of the makefile
and comment the COMPILER line with gfortran, and uncomment
the COMPILER line with ifort.


### Building on Windows

There are 2 makefiles provided to build on Windows. One is
a GNU style makefile to use with, for example, mingw32-make, 
or with the nmake program if you have nmake installed. Edit
the Windows makefile of your choice to set the COMPILER line
to the compiler you are using.

```
nmake -fMakefileNMWIN all
```

If successful, the binaries are located in the build directory.
Follow the instructions in the Theriak-Domino manual in the
dist/docs directory to install on Windows.

### Building on Linux (Ubuntu)

To be written... 

### Known Compiler & OS Compatibility
The program suite has recently been built and run using 
the OS, processor, and compiler combinations shown below. 

| Platform       | Version   | Compiler    | Processor     |   |
|----------------|-----------|-------------|---------------|---|
| MacOS          | 11.x-12.x | gfortran    | Intel         |   |
| MacOS          | 11.x      | Intel ifort | Intel         |   |
| MacOS          | 12.x      | gfortran    | Apple Silicon |   |
| Windows        | 10        | Intel ifort | Intel         |   |
| Windows        | 10        | Intel ifx   | Intel         |   |
| Windows        | 10        | gfortran    | Intel         |   |
| Linux (Ubuntu) | 22        | Intel ifort | Intel         |   |
| Linux (Ubuntu) | 22        | Intel ifx   | Intel         |   |

