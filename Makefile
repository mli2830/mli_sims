### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: simulate.mcmc.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md journal.txt
include stuff.mk

##################################################################

## Content

Sources += $(wildcard *.R)

simulate.Rout: simulate.R

simulate.mcmc.Rout: simulate.Rout mcmc.R
%.mcmc.Rout: %.Rout mcmc.R
	$(run-R)

##################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
# Makefile: start.makestuff


-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk

-include rmd.mk
