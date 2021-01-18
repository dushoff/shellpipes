# shellpipes

An R package for working through Makefile and command lines. Designed to be used with [makestuff](https://github.com/dushoff/makestuff) and specifically with [pipeR.mk](https://github.com/dushoff/makestuff/blob/master/pipeR.mk)

## Installation

From inside R:

`remotes::install_github("dushoff/shellpipes")`

This will fail for old versions of "remotes", because the default branch is now "main" instead of "master". `, ref="main"` before the last paren may well fix this problem, but probably what you want to do is update remotes:

`install_packages("remotes")`

… and then try again

## Overview

