### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: multivariate.html

##################################################################

# make files

Sources = Makefile .gitignore README.md LICENSE.md journal.txt

##################################################################

## Content

#### Li

Sources += $(wildcard *.rmd)
Archive += multivariate.html

multivariate.html: multivariate.rmd

done:
	say -v Trinoids "complete"

job: multivariate.html done

-include rmd.mk


