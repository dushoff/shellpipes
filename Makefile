## This is rpipes

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += DESCRIPTION .Rbuildignore NAMESPACE
Sources += $(wildcard R/*.R)

## R/gen.R (generalized exp/log)
## R/kernel.R deal with discretized kernels

## pkgall:

######################################################################

Sources += $(wildcard dev/*.R)

dev/gen.Rout: dev/gen.R
	$(makeR)

######################################################################

### Makestuff

Sources += Makefile

Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/makeR.mk
-include makestuff/rpkg.mk

-include makestuff/git.mk
-include makestuff/visual.mk
