% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{manageConflicts}
\alias{manageConflicts}
\title{Set conflicts to trigger an error, and provide a way to work around them
Assume conflicts with base packages are OK by default}
\usage{
manageConflicts(
  base = c("base", "methods", "utils", "grDevices", "graphics", "stats"),
  add = NULL
)
}
\arguments{
\item{base}{is the base package list}

\item{add}{is an additional package list}
}
\description{
Set conflicts to trigger an error, and provide a way to work around them
Assume conflicts with base packages are OK by default
}
