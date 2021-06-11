## This is shellpipes
## https://dushoff.github.io/shellpipes/

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt R/files.R R/graphics.R R/read.R R/save.R"

######################################################################

## pkgall:
## quickinstall:

######################################################################

Sources += $(wildcard dev/*.R)

set.Rout: dev/set.R
	$(makeR)

look.Rout: dev/look.R set.rda

plot.Rout: dev/plot.R set.rda 
	$(makeR)

######################################################################

### Makestuff

Sources += Makefile notes.txt README.md index.md

Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/rpkg.mk

-include makestuff/git.mk
-include makestuff/visual.mk
