# shellpipes

An R package for working through Makefile and command lines. Designed to be used with [makestuff](https://github.com/dushoff/makestuff) and specifically with [pipeR.mk](https://github.com/dushoff/makestuff/blob/master/pipeR.mk) (see [code](https://github.com/dushoff/shellpipes.git))

## Installation

From inside R:

`remotes::install_github("dushoff/shellpipes")`

This will fail for old versions of "remotes", because the default branch is now "main" instead of "master". `, ref="main"` before the last paren may well fix this problem, but probably what you want to do is update remotes:

`install_packages("remotes")`

… and then try again

_Alternative_ If you can't install_github because of permission problems, try:

* [download this zip file](https://github.com/dushoff/shellpipes/archive/refs/heads/main.zip)
* unzip it
* `R CMD INSTALL shellpipes-main` from the directory where you unzipped.

## Overview

