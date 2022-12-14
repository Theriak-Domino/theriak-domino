# Theriak-Domino Repository

# Download and Install

# Building from Source Code
Git clone the repository as described above. The src
directory contains several makefiles to choose from
depending on your OS and compiler. OS and compiler 
compatibility are shown in the table in the section below
on compatibility.


## Building on MacOS, Apple Silicon Processor
For macs equipped with an Apple Silicon processor, the makefile 
provided in the src directory requires gfortran to compile and
build the suite of programs (see the next section if your mac 
is equipped with an Intel processor). If you do not have gfortran
installed, I recommend installing using HomeBrew. From the 
Terminal window, navigate to the src directory. To build all 
targets, type:

make -fMakefileMAC all

This will build all binaries and place them in the 'build' 
directory, alongside the src directory. This directory will 
automatically be created when it does not exist. If you receive 
an error regarding modules, run the clean target first to 
remove all pre-existing *.mod files, if any:

make -fMakefileMAC clean

make -fMakefileMAC all


## Building on MacOS, Intel Processor
Both the gfortran compiler and the Intel ifort compiler 
can compile and build the suite of programs on Intel macs.


## Building on Windows


## Building on Linux (Ubuntu)


## Known Compiler & OS Compatibility
The program suite has recently been built and run using 
the OS, processor, and compiler combinations shown below. 
| Platform       | Version | Compiler    | Processor     |   |
|----------------|---------|-------------|---------------|---|
| MacOS          | 11.-12. | gfortran    | Intel         |   |
| MacOS          | 11.     | Intel ifort | Intel         |   |
| MacOS          | 12.     | gfortran    | Apple Silicon |   |
| Windows        |         | Intel ifort | Intel         |   |
| Windows        |         | Intel ifx   | Intel         |   |
| Windows        |         | gfortran    | Intel         |   |
| Linux (Ubuntu) | 22      | Intel ifort | Intel         |   |
| Linux (Ubuntu) | 22      | Intel ifx   | Intel         |   |

