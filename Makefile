### Hooks for the editor to set the default target
current: fastmatcomputation.html
	open fastmatcomputation.html
	say -v Vicki "complete"

target pngtarget pdftarget vtarget acrtarget: pymc.html

##################################################################

# make files

Sources = Makefile .gitignore README.md LICENSE.md journal.txt

##################################################################

## Content

#### Li

Sources += $(wildcard *.rmd)
Archive += multivariate.html

%.html: %.rmd
	say -v Trinoids "complete"

pymc.html: pymc.rmd

test: pymc.html
	open pymc.html
	say -v Trinoids "complete"

pymc2.html: pymc2.rmd

-include rmd.mk


