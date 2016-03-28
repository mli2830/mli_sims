### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: simulate.mcmc.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md journal.txt
include stuff.mk

##################################################################

## Content

#### Li

Sources += $(wildcard *.rmd)
Archive += multivariate.html

multivariate.html: multivariate.rmd

##### Dushoff
Sources += $(wildcard *.R)

naive.simulate.Rout: simulate.R
lme4.simulate.Rout: lme4_sim_params.R lme4_sim.R
%.simulate.Rout:
	$(run-R)

%.mcmcglmm_fit.Rout: %.Rout mcmcglmm_fit.R
		     $(run-R)

%.lme4_fit.Rout: %.Rout lme4_fit.R
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
