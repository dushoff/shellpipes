#' Source certain files from a file list
#' @param fl file list to select from (makeArgs by default)
#' @param exts extensions to select
#' @param first (Boolean); should we use the first matched file (usually the master script)?
#' @param verbose (Boolean); passed to source()
#' @export
sourceFiles <- function(fl=makeArgs() 
	, exts=c("R", "r"), first=FALSE, verbose=FALSE)
{
	fl <- fileSelect(fl, exts)
	if (!first) fl <- fl[-1]
	for (f in fl){
		source(f, verbose=verbose)
	}
}

#' Read environments from a file list to a single environment
#' @param fl file list to select from (makeArgs by default)
#' @param exts extensions to select
#' @param parent (defaults to parent.frame())
#' @export
commandEnvironments <- function(fl = makeArgs()
	, exts = c("RData", "rda", "rdata"), parent=parent.frame()
)
{
	envl <- fileSelect(fl, exts)
	loadEnvironments(envl, parent)
	invisible(envl)
}

#' get a single environment using matchFile, load it and return it
#' @param pat pattern to macth
#' @param fl file list to select from (makeArgs by default)
#' @param exts extensions to select
#' @export
getEnvironment <- function(pat="", fl = makeArgs()
	, exts = c("RData", "rda", "rdata")
)
{
	ff <- fileSelect(fl, exts)
	f <- matchFile(pat, ff)
	e <- new.env()
	load(f, e)
	return(e)
}

#' Load a list of environments
#' @param envl a list of environments
#' @param parent (defaults to parent.frame())
#' @export
loadEnvironments <- function(envl, parent=parent.frame())
{
	for (env in envl){
		load(env, parent)
	}
}

#' read environment files and return a list of environments
#' @param pat pattern to match
#' @param fl file list to select from (makeArgs by default)
#' @param exts extensions to select
#' @param names a list of names for the environments found
#' @param trim a pattern for making names from the file names
#' @export
loadEnvironmentList <- function(pat = NULL
	, fl = makeArgs()
	, exts = c("RData", "rda", "rdata")
	, names=NULL, trim = "\\.[^.]*$"
)
{
	envl <- fileSelect(fl, exts, pat)
	if(is.null(names)){
		names = sub(trim, "", envl)
	}
	stopifnot(length(names)==length(envl))
	el <- list()
	if(length(envl)==0)
	{
		warning("No environments matched in loadEnvironmentList")
		return(NULL)
	}
	for (i in 1:length(envl)){
		el[[i]] <- new.env()
		load(envl[[i]], el[[i]])
	}
	names(el) <- names
	return(el)
}

## FIXME Case-insensitive extensions
## FIXME csvRead etc. as wrappers
tableRead <- function(pat=NULL, delim=" ", exts=c("csv", "CSV", "ssv", "scsv", "tsv")
	, fl = makeArgs(), ...
){
	return(readr::read_delim(matchFile(pat, fl, exts), delim, ...))
}

csvRead <- function(pat=NULL, exts=c("csv", "CSV")
	, fl = makeArgs(), ...
){
	return(readr::read_csv(matchFile(pat, fl, exts), ...))
}

tsvRead <- function(pat=NULL, exts=c("tsv", "TSV")
	, fl = makeArgs(), ...
){
	return(readr::read_tsv(matchFile(pat, fl, exts), ...))
}

## Not tested! Is it important?
csvReadList <- function(pat=NULL, exts=c("csv", "CSV")
	, fl = makeArgs(), ...
	, names=NULL, trim = "\\.[^.]*$"
){
	fl <- fileSelect(fl, exts, pat)
	if(is.null(names)){
		names = sub(trim, "", fl)
	}
	stopifnot(length(names)==length(fl))
	csvl <- lapply(fl
		, function(fn){readr::read_csv(fn, ...)}
	)
	names(csvl) <- names
	return(csvl)
}

