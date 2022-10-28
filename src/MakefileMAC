PLAT = platf-osx
OBJECTS = activi.o dasave.o dbread.o fsol.o gcalc.o gmini.o hprogs.o prinin.o prtcal.o help.o $(PLAT).o
COMP90 = gfortran -O2

##COMP90 = gfortran -O2 -fbounds-check -Wunused
##COMP90 = gfortran -g -fbacktrace -ffpe-trap=underflow

COMP77 = gfortran -O2
SEX = f90

activi.o : activi.$(SEX) theriak.cmn files.cmn
	 $(COMP90) -c activi.$(SEX)
    
dasave.o : dasave.$(SEX) theriak.cmn
	$(COMP90) -c dasave.$(SEX)
    
dbread.o : dbread.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c dbread.$(SEX)
    
fsol.o : fsol.$(SEX)
	$(COMP90) -c fsol.$(SEX)
    
gcalc.o : gcalc.$(SEX) theriak.cmn
	$(COMP90) -c gcalc.$(SEX)

gmini.o : gmini.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c gmini.$(SEX)

hprogs.o : hprogs.$(SEX)
	$(COMP90) -c hprogs.$(SEX)

prinin.o : prinin.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c prinin.$(SEX)

prtcal.o : prtcal.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c prtcal.$(SEX)

help.o : help.$(SEX) files.cmn thblock.cmn
	$(COMP90) -c help.$(SEX)

$(PLAT).o : $(PLAT).$(SEX)
	$(COMP90) -c $(PLAT).$(SEX)

theriak.o : theriak.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c theriak.$(SEX)

theriak : theriak.o $(OBJECTS)
	$(COMP90) -o theriak theriak.o $(OBJECTS)

theriakd.o : theriakd.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c theriakd.$(SEX)

theriakd : theriakd.o $(OBJECTS)
	$(COMP90) -o theriakd theriakd.o $(OBJECTS)

domino.o : domino.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c domino.$(SEX)

domino : domino.o $(OBJECTS)
	$(COMP90) -o domino domino.o $(OBJECTS)

guzzler.o : guzzler.$(SEX)
	$(COMP90) -c guzzler.$(SEX)

guzzler : guzzler.o $(PLAT).o
	$(COMP90) -o guzzler guzzler.o $(PLAT).o

explot.o : explot.$(SEX) expl.cmn
	$(COMP90) -c explot.$(SEX)

explot : explot.o $(PLAT).o
	$(COMP90) -o explot explot.o $(PLAT).o

therbin.o : therbin.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c therbin.$(SEX)

therbin : therbin.o $(OBJECTS)
	$(COMP90) -o therbin therbin.o $(OBJECTS)

therbinc.o : therbinc.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c therbinc.$(SEX)

therbinc : therbinc.o $(OBJECTS)
	$(COMP90) -o therbinc therbinc.o $(OBJECTS)

therter.o : therter.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c therter.$(SEX)

therter : therter.o $(OBJECTS)
	$(COMP90) -o therter therter.o $(OBJECTS)

thermo.o : thermo.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c thermo.$(SEX)

thermo : thermo.o activi.o dasave.o dbread.o fsol.o gcalc.o hprogs.o help.o $(PLAT).o
	$(COMP90) -o thermo thermo.o activi.o dasave.o dbread.o fsol.o gcalc.o hprogs.o help.o $(PLAT).o
    
thalia.o : thalia.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c thalia.$(SEX)

thalia : thalia.o activi.o dasave.o dbread.o fsol.o gcalc.o hprogs.o help.o $(PLAT).o
	$(COMP90) -o thalia thalia.o activi.o dasave.o dbread.o fsol.o gcalc.o hprogs.o help.o $(PLAT).o

theriaq.o : theriaq.$(SEX) theriak.cmn files.cmn aqua.cmn
	$(COMP90) -c theriaq.$(SEX)

theriaq : theriaq.o $(OBJECTS)
	$(COMP90) -o theriaq theriaq.o $(OBJECTS)

therieq3.o : therieq3.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c therieq3.$(SEX)

therieq3 : therieq3.o $(OBJECTS)
	$(COMP90) -o therieq3 therieq3.o $(OBJECTS)

makemap.o : makemap.$(SEX) files.cmn map.cmn
	$(COMP90) -c makemap.$(SEX)

makemap : makemap.o hprogs.o help.o $(PLAT).o
	$(COMP90) -o makemap makemap.o hprogs.o help.o $(PLAT).o

theriag.o : theriag.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c theriag.$(SEX)

theriag : theriag.o $(OBJECTS)
	$(COMP90) -o theriag theriag.o $(OBJECTS)

reafit.o : reafit.$(SEX) theriak.cmn files.cmn fitpar.cmn thfit.cmn
	$(COMP90) -c reafit.$(SEX)

reafit : reafit.o $(OBJECTS)
	$(COMP90) -o reafit reafit.o $(OBJECTS)

vercheck : vercheck.$(SEX)
	$(COMP90) -o vercheck vercheck.$(SEX)

sublist : sublist.$(SEX)
	$(COMP90) -o sublist sublist.$(SEX)

lincount : lincount.$(SEX)
	$(COMP90) -o lincount lincount.$(SEX)

plotxy.o : plotxy.$(SEX) files.cmn
	$(COMP90) -c plotxy.$(SEX)

plotxy : plotxy.o hprogs.o help.o $(PLAT).o
	$(COMP90) -o plotxy plotxy.o hprogs.o help.o $(PLAT).o

plotg.o : plotg.$(SEX) files.cmn
	$(COMP90) -c plotg.$(SEX)

plotg : plotg.o hprogs.o help.o $(PLAT).o
	$(COMP90) -o plotg plotg.o hprogs.o help.o $(PLAT).o

intri : intri.$(SEX)
	$(COMP90) -o intri intri.$(SEX)

therisub.o : therisub.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c therisub.$(SEX)

subtest.o : subtest.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c subtest.$(SEX)

subtest : subtest.o therisub.o $(OBJECTS)
	$(COMP90) -o subtest subtest.o therisub.o $(OBJECTS)

therion.o : therion.$(SEX) theriak.cmn files.cmn checkdb.cmn
	$(COMP90) -c therion.$(SEX)

therion : therion.o $(OBJECTS)
	$(COMP90) -o therion therion.o $(OBJECTS)

jphases.o : jphases.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c jphases.$(SEX)

jphases : jphases.o $(OBJECTS)
	$(COMP90) -o jphases jphases.o $(OBJECTS)

jfiles.o : jfiles.$(SEX) theriak.cmn files.cmn
	$(COMP90) -c jfiles.$(SEX)

jfiles : jfiles.o $(OBJECTS)
	$(COMP90) -o jfiles jfiles.o $(OBJECTS)

cat2.o : cat2.$(SEX)
	$(COMP90) -c cat2.$(SEX)

cat2 : cat2.o hprogs.o $(PLAT).o
	$(COMP90) -o cat2 cat2.o hprogs.o $(PLAT).o

pretherion.o : pretherion.$(SEX)
	$(COMP90) -c pretherion.$(SEX)

pretherion : pretherion.o $(PLAT).o
	$(COMP90) -o pretherion pretherion.o $(PLAT).o

all : theriak domino guzzler explot therbin therter thermo thalia makemap theriag plotxy plotg jfiles jphases\
 cat2 theriakd therion pretherion

