CHANGES IN THERIAK-DOMINO version 2023.03.06
--------------------------------------------
- A beta version of this release was previously released as
2023.01.02 beta, with the changes listed below
- Theriak-Domino source repository moved to Git Hub; added 
GPLv3 license
- introduced a few flags to control what iteration loop SEEDS are
added to the minimization
- implemented changes to allow significantly lower phase component
proportions and site element fractions in complex ax models,
that otherwise leads to minimization failures for some models at
certain conditions.
- updates to identify and correct underflow and invalid exceptions
in certain places (a few places still to clean up); updated 
parameters in theriak.ini for use with this version
- updated HP11 thermo expressions for compatibility with tc350si;
G's match to better than 1000'th of J.
- added DGC data line to database file format to mimic tc dqf
better.
- added a CALC-FLAGS section to theriak.ini that can take
additional parameters;  OUTFLAGS can be set to true to output
active CALC-PARAMETERS used during calculation of dominograms;
(other flags that can be set in the .ini file are found in the 
flags source file but generally should not be changed)
- fixed svg output in guzzler/explot to display correctly in 
web browser.
- update makefiles
    - updated makefiles to allow selection of compilers with preset
flags; 
    - makefile for osx now allows static linking with gfortran
to remove binary dependency on dynamic gcc/gfort libs; 
    - added a makefile for linux that works with gfortran/ifort/ifx 
(only tested on ubuntu 22); 
    - versioning string added to top of makefile to allow specifying 
versioning during building; this allows outputting the build version 
during program execution that is useful when you have multiple builds
installed on your system;
    - osx makefile allows use of gfortran or ifort on intel macs, 
gfortran only on apple silicon;
    - on Windows, added an NMake formatted makefile so windows builds
don't require gcc/gfort to be installed to build from a provided
makefile (for e.g., can use nmake (visual studio) with ifort/ifx)
    - a CMake based build system is being worked on for the next 
release
- added additional dataset files (converted from THERMOCALC format to 
Theriak-Domino format).
- renamed and added new liquid models to fsol.f90, (attempting
for consistent naming). The Tomlinson & Holland (2021) liquid
model is not working 100% correctly yet so don't use it (can generally
give a good idea of relative equilibrium assemblage sequence, but it 
does not exactly match THERMOCALC results like the other models do)

***
CHANGES IN THERIAK-DOMINO version 28.05.2022
--------------------------------------------
Only some minor changes in the main programs.

Theria_g and theriakd are no longer part of the distribution. See the pages by their 
respective authors:
Theria_g: https://www.teamgar.net/fred-gaidies and https://www.teamgar.net/theria_g
Theriakd: https://www.rocks.uni-kiel.de/theriakd/ 
          and https://www.rocks.uni-kiel.de/theriakd/html/down_en.html

***
CHANGES IN THERIAK-DOMINO version 11.03.2020
--------------------------------------------
Some improvement in minimizations. Still problems with zero concentrations.

Applied some conventions to satisfy the prigs(*) and pedants(**) of gfortran 9.2:
(mostly variable declarations and loop definitions.)

The length of phase names, abbreviations and solution names are tested, and a warning is
printed if they are too long. Up to now the automatic truncation did not really matter,
however ...
NAME(i)   16 characters (Names of phases)
ABK(i)     8 characters (Abbreviation of phases)
SOLNAM(i) 16 characters (Name of solution phases)

Updated the Theriak-Domio Guide Moved information depending on operating systems to
"readme" and the homepage. Corrected the most embarrassing typos.

Corrected some logical inconsistencies(***) in Theriak output.

***
Changes in THERIAK-DOMINO version 09.03.2019
--------------------------------------------
Even more confusion is added. Because of my input files the '-' in the variable CODE is
back to exclude phases from becoming stable. '+' means the phases are calculated for use
in 'COM" lines, even if they are not in the compositional space.

Initial guesses can be easily added in the database. The format is the same as written at
the end of theriak-runs. 
There is some improvement in the minimization. The third parameter in the top line of the
database can be 0 (=no scan), 1 (scan with x=1/2) or 2 (scan with x=1/4)

***
Changes in THERIAK-DOMINO version 04.02.2017
--------------------------------------------
Some confusion added to the database. Due to a misunderstanding the meaning of the
'+' in the variable CODE changed in order to be compatible with Doug Tinkham's coding
of Landau Phases. So: using '+' will exclude the phase from becoming stable.
If the phase should not be excluded, use '-'

***
Changes in THERIAK-DOMINO version 09.03.2016
--------------------------------------------
Some minor changes.
Equation of state for melts is now working properly. (Thanks to Doug Tinkham)

***
Changes in THERIAK-DOMINO version 11.02.2015
--------------------------------------------
Solution models White et al. 2014 included in database.
Warning: Equation of state for melts is not working properly. Is OK up to medium pressures. {??)

***
Changes in THERIAK-DOMINO version 03.01.2012
--------------------------------------------
Coefficients for the asymmetric van Laar model, according to Holland and Powell (2002): If a0 is negative, the asymmetry size parameter used is the relative volume of the end-members at P & T. (For fluids)

Equation of state according to Holland an Powell (2011):
new volume function (Holland and Powell 2011)  V11
Bragg-Williams (Holland and Powell 2011) BW1
Landau (Holland and Powell 2011)  LA1
dk0dt for liquids (Holland and Powell 2011)   DK1
EOS for water: K.S. Pitzer and S.M. Sterner, 1994   PS94H2O 
EOS for CO2: K.S. Pitzer and S.M. Sterner, 1994   PS94CO2

In some cases where an explicit Volume is not required to calculate ∆G, the volumes were previously not calculated.
Now all volumes are calculated explicitly or, in some cases with (dG/dP).

The number of endmembers in the external subroutine is no longer restricted to 10. (is now defined as 15, but can "easily" be changed.

The phases used for a calculation can be defined in the input line for the database.
Example:
Enter [ "?" | CR | "files" | database filename ] <JUN92.bs>?
JUN92.bs    Albite   Jadeite   a-Quartz   b-Quartz
Of all phases possible for the given bulk composition, this will use only Albite, Jadeite, a-Quartz and b-Quartz.
(useful to calculate e.g. a single reaction. Note the two blanks between items)


Provision to use initial guesses for the compositions of stable solutions.
Still experimental. Filneame=inimini
example:
SOLUTION   
ClAMP   tremolite     tschermakite2   pargasite2    glaucophane2   cummingtonite2   grunerite2     acam           bcam          mgriebekite
        7.76376D-01   3.88098D-02     7.53481D-02   2.17005D-02    5.63273D-03      1.00090D+00   -5.50374D-01   -3.68393D-01   0.000000D0
        4.21530D-02   3.04170D-02     4.10399D-03   7.39931D-03    4.11153D-02      3.63996D-01    1.87278D-01    3.23537D-01   0.000000D0

In domino: smaller step along grid (ptdist) may be defined (not recommended)

***
Changes in THERIAK-DOMINO version 010809
----------------------------------------
Simplified input for isolines
Addition of "REF" in drv-files.
New program plotxy for tab-files.
Excluding phases and monitoring their chemical potential with EXCL and $
Definition of thermal gradient in input.
Models including site-margules, new section in database.

***
Changes in THERIAK-DOMINO version 150508
----------------------------------------
possibility to distinguish solution phases by the dominant endmember.
(key "-1" in labeling of reactions)
minor changes, e.g. "elements in stable phases" in output of Theriak.
Distribution contains Theriag (see special documentation).

***
Changes in THERIAK-DOMINO version 111207
----------------------------------------
Made compilable with fortran90. (gfortran):
Comments with "!", continuation lines with "&" at end of line.
Changed loops with real DO indices to "DO WHILE" loops.

***
Changes in THERIAK-DOMINO version 200307
----------------------------------------
Several changes were made in the algorithm, to improve robustness in complex solutions. As a 
result, the calculations become considerably slower than with previous versions. I consider
robustness far more important than speed, and because computers are becoming faster each
year, this slowing is of minor concern.

Solutions may include an asymmetry van Laar parameter, according to Holland and Powell(2002).

Solutions that may have negative concentrations of their components must now be marked with
a "-" in the solution definition line. Default is: all endmembers are >= 0.

Lists or phases etc. are now in alphabetical order.

In the section "activity of all phases", not all unstable solution phases are printed.

The "pix" function in domino has now be implemented. This allows to calculate phase diagrams
on a fixed grid (e.g. 100x100) and then to plot graymap images of any desired variable.
The program to produce the grey-scale images, "makemap" is included.

***
Changes in THERIAK-DOMINO version ....06
----------------------------------------

Significant contributions to increase user friendliness and code portability were made
by K. Petrakakis. The most important changes are:
- Help function. (Interactive input by the user includes a '?', which shows detailed
  explanations for each option. The text of this help-function is stored in the file
  thhelp.txt)
- Flexible file-assignments. (most file names for input and output are defined in the
  file theriak.ini. Users may change these to their taste)
- A few platform-dependent subroutines (e.g. the setting of environmental variables) 
  are provided in the files platfxxx.f. The rest of the programs and subroutines are 
  completely platform independent. This structure increased portability of the programs 
  to the most common operating systems allowing for preparation of binaries for download.

Further changes in code include:
- Input numbers (e.g. in the chemical formulas or COM-line) may include fractions (x/y).
  e.g. the definition of ordered biotite may be written as:
  COM    phlogopite[2/3]annite[1/3]

- The Database now includes the stoichiometric oxygen numbers based on the commonly used 
  standard oxides. This allows to define the total amount of O (Oxygen) in bulk composition 
  definition (in dat-file) as O(?). In this case the standard oxidation state as defined 
  by the stoichiometric oxygen amount defined in the database is used to calculate the 
  total O, e.g. AL(2)SI(1)O(?) is equivalent to: AL(2)SI(1)O(5). 

- The calculation parameters (like TEST or LO1MAX) are now defined in the file theriak.ini. 
  Earlier they were defined in the dat-file (e.g. THERIN).

- The parameter PGAS (ratio (solid pressure)/(liquid pressure) can be defined in the 
  dat-file as optional input following P. This was already implemented in earlier versions. 

- Definition of solutions: The site multiplicity m of a solution is now defined as a number 
  directly following the right bracket of the model-keywords. The default is m = 1, but may 
  also be a fraction like 1/3 etc. 
  Example of current garnet model definition: 
  GARNET    (MARGULES,IDEAL)3   M(3):Ca,Mg,Fe,Mn. 

  Now the keyword IDEAL means strictly that activity(i) = mole fraction(i)**m. 
  (xi = concentration of endmember i, m = multiplicity as defined above)
  Note: this is a major change. In older versions ideal solutions with a multiplicity other 
  than one had to be defined as "SITE" or "IDEAL" with one site defined, for eample as      
     SOL-NAME    (IDEAL)     M1(3):Fe,Mg
  These definitions must now be changed to:     
     SOL-NAME    (IDEAL)3    M1(3):Fe,Mg. 
  The site definition in "IDEAL" solutions is ignored for the calculation. If present, it 
  will be used to print site occupancies in the output.

***
Changes in THERIAK-DOMINO version 140205
----------------------------------------

The minimization routine has been adapted to allow for some endmembers of certain 
solutions to have negative concentrations. Details: If the activity of an endmember 
is not zero although its concentration is zero, this endmember is assumed to describe 
ordering and thus may have negative concentrations. This definition allows the program 
to handle solution models with "proportions" (as defined by R. Powell and T. Holland 
for THERMOCALC) as normal endmember solutions. Warning: This feature is not extensively 
tested. Users are advised to think. 

***
Changes in THERIAK-DOMINO version 200704
----------------------------------------
Increased robustness of code. Added a Newton-Raphson to the gradient search.
Check rounding errors in finding minimum assemblage.
Result of these changes is a substantially increased computing time. (ca. 2x).
Considering the advantages of the increased robustness and the increasing speed of
newer computers this is a minor concern.

------------------------------------------------------------------------------------------
(*) a self-righteously moralistic person who behaves as if they are superior to others.
(**) a person who is excessively concerned with minor details and rules or with displaying
academic learning.
(***) blatant mistakes
