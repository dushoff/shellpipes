## This is shellpipes
## https://dushoff.github.io/shellpipes/
## https://github.com/dushoff/shellpipes.git

newversion: pull
	$(MAKE) quickinstall

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt index.md README.md R/files.R R/graphics.R R/read.R R/save.R"

######################################################################

## pkgall:
## quickinstall:

Ignore += shellpipes*.tar.gz

######################################################################

Sources += $(wildcard dev/*.R)

## mess around with ellipsis
dots.Rout: dev/dots.R
	$(pipeR)

## testing
set.Rout: dev/set.R
	$(pipeR)

look.Rout: dev/look.R set.rda

plot.Rout: dev/plot.R set.rda 
	$(pipeR)

######################################################################

## index.gh.html: index.md

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
-include makestuff/pandoc.mk

-include makestuff/git.mk
-include makestuff/visual.mk
