### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: multivar_sim.md
##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md journal.txt
include stuff.mk

##################################################################

##################################################################

## Content

### Makestuff

## Change this name to download a new version of the makestuff directory
# Makefile: start.makestuff


-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
