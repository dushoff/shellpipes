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

######################################################################

# rds files

#' Read an rds file matched from a list of files
#' @param pat optional string pattern to match
#' @param exts extensions for the table file
#' @param fl file list (defaults to command arguments)
#' @param refhook pass to readRDS
#' @export
rdsRead <- function(pat=NULL, exts=c("rds", "Rds", "RDS")
	, fl = makeArgs(), refhook=NULL
){
	return(readRDS(matchFile(pat, fl, exts), refhook))
}

#' read rds files and return a list of objects
#' @param pat pattern to match
#' @param fl file list to select from (makeArgs by default)
#' @param exts extensions to select
#' @param names a list of names for the environments found
#' @param trim a pattern for making names from the file names (not used when names if provided)
#' @param refhook pass to readRDS
#' @export
rdsReadList <- function(pat = NULL
	, fl = makeArgs()
	, exts = c("rds", "RDS")
	, names=NULL, trim = "\\.[^.]*$"
	, refhook=NULL
)
{
	rdsl <- fileSelect(fl, exts, pat)
	if(is.null(names)){
		names = sub(trim, "", rdsl)
	}
	stopifnot(length(names)==length(rdsl))
	rl <- list()
	if(length(rdsl)==0)
	{
		warning("No files matched in rdsReadList")
		return(NULL)
	}
	for (i in 1:length(rdsl)){
		rl[[i]] <- readRDS(rdsl[[i]], refhook)
	}
	names(rl) <- names
	return(rl)
}

######################################################################

# environment (rda) files

#' Read environments from a file list to a single environment
#' @param fl file list to select from (makeArgs by default)
#' @param exts extensions to select
#' @param parent (defaults to parent.frame())
#' @export
loadEnvironments <- function(fl = makeArgs()
	, exts = c("RData", "rda", "rdata"), parent=parent.frame()
)
{
	envl <- fileSelect(fl, exts)
	for (env in envl){
		load(env, parent)
	}
	invisible(envl)
}

#' Read environments from a file list to a single environment
#' Deprecated name (a trivial wrapper now for loadEnvironments)
#' FIXME should just be an alias, if I understood rdnames
#' @param ... parameters to pass to loadEnvironments
#' @param parent get and pass on our parent
#' @export
commandEnvironments <- function(..., parent=parent.frame()){
	loadEnvironments(..., parent=parent)
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

######################################################################

## readr stuff
## FIXME Case-insensitive extensions
## FIXME csvRead etc. as wrappers
## FIXME default delimiter table

#' read a table matched from a list of files
#' @param pat optional string pattern to match
#' @param delim delimiter for reading files
#' @param exts extensions for the table file
#' @param fl file list (defaults to command arguments)
#' @param ... additional parameters for read_delim
#' @export
tableRead <- function(pat=NULL, delim=" ", exts=c("csv", "CSV", "ssv", "scsv", "tsv")
	, fl = makeArgs(), ...
){
	return(readr::read_delim(matchFile(pat, fl, exts), delim, ...))
}

#' read a csv matched from a list of files
#' @param pat optional string pattern to match
#' @param exts extensions for the table file
#' @param fl file list (defaults to command arguments)
#' @param ... additional parameters for read_csv
#' @export
csvRead <- function(pat=NULL, exts=c("csv", "CSV")
	, fl = makeArgs(), ...
){
	return(readr::read_csv(matchFile(pat, fl, exts), ...))
}

#' read a tsv matched from a list of files
#' @param pat optional string pattern to match
#' @param exts extensions for the table file
#' @param fl file list (defaults to command arguments)
#' @param ... additional parameters for read_tsv
#' @export
tsvRead <- function(pat=NULL, exts=c("tsv", "TSV")
	, fl = makeArgs(), ...
){
	return(readr::read_tsv(matchFile(pat, fl, exts), ...))
}

#' read csv files into a list
#' @param pat optional string pattern to match
#' @param exts extensions for the table file
#' @param fl file list (defaults to command arguments)
#' @param ... additional parameters for read_csv
#' @param names for the new list
#' @param trim if names are not given, take stuff off the end of the matched filenames (defaults to the last "." and everything thereafter)
#' @export
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

