### Hooks for the editor to set the default target
current: pymc.html
	open pymc.html
	say -v Vicki "Human, your file is complete"

target pngtarget pdftarget vtarget acrtarget: pymc.html

##################################################################

# make files

Sources = Makefile .gitignore README.md LICENSE.md journal.txt

##################################################################

## Content

#### Li

Sources += $(wildcard *.rmd)
Archive += multivariate.html

multivariate.html: multivariate.rmd
	say -v Trinoids "complete"

pymc.html: pymc.rmd

-include rmd.mk


