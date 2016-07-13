### Hooks for the editor to set the default target
target: TSIR.html

target pngtarget pdftarget vtarget acrtarget: TSIR.html

##################################################################

# make files

Sources = Makefile .gitignore README.md LICENSE.md journal.txt

##################################################################

## Content

#### Li

Sources += $(wildcard *.rmd)
Archive += multivariate.html

pymc.html: pymc.rmd
pymc2.html: pymc2.rmd
contrasts.html: contrasts.rmd
%.html: %.rmd
	say -v Trinoids "complete"

contrasts.html.go: contrasts.html
		   firefox contrasts.html

-include rmd.mk


