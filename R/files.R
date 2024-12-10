#' Get a targetname
#' @param ext file extension for output
#' @param suffix file extension of provided name (.Rout by default)
#' @param fn provided file name (first of commandArgs by default)
#' @return a filename to write something to
#' @export
targetname <- function(ext="", suffix="\\.Rout", fn = makeArgs()[[1]]){
	return(sub(suffix, ext, fn))
}

#' Try to find a single file matching a given description (usually for reading)
#' @param pat is a pattern for matching
#' @param fl is the list of filenames to search (makeArgs by default)
#' @param exts is a list of allowed extensions
#' @export
matchFile <-  function(pat=NULL, fl = makeArgs(), exts=NULL){
	f <- fileSelect(fl, exts, pat)
	err <- ""
	if (length(f) == 0) err <- "No match"
	if (length(f) > 1) err <- "More than one match"
	if (err=="") return(f)
	stop("matchFile: ", err, " in makeArgs [ ", pat, "] -- ", paste(fl, collapse=" "))
}

#' Return the starred text passed by make
#' @param tag identifies the passed text
#' @param fl is the list of filenames to search (makeArgs by default)
#' @export
pipeStar <-  function(tag="pipestar", fl = makeArgs()){
	f <- matchFile(fl=fl, exts=tag)
	return (sub(paste0(".", tag), "", f))
}

rpipesenv <- new.env()
rpipesenv$callArgs <- NULL

#' set callArgs for interactive use
#' @param call command-line call as generated in .Rout.args
#' @export
rpcall <- function(call){
	rpipesenv$callArgs <- call
	invisible(call)
}

#' Not exported: makeArgs
#' A service function to get the make arguments
#' when R was called interactively, these come from a variable called callArgs
#' otherwise parsed from the command line
makeArgs <- function(){
	if ("shellpipes" %in% commandArgs(TRUE))
		return(commandArgs(TRUE))
	if (is.null(rpipesenv$callArgs))
		stop("Define callArgs to use makeR files; see .args file?")
	return(strsplit(rpipesenv$callArgs, " ")[[1]])
}

#' Select a list of filenames from a list by extension
#' … and optionally by pattern as well
#' @param fl filelist (makeArgs by default)
#' @param exts a list of extensions to match (. is added here)
#' @param pat a pattern to filter by
#' @export
fileSelect <- function(fl = makeArgs(), exts=NULL, pat=NULL)
{
	if(!is.null(exts)){
		outl <- character(0)
		for (ext in exts){
			if(grepl("\\.", ext))
				warning("Extension", ext, "starts with . in fileSelect")
			ss <- paste0("\\.", ext, "$")
			outl <- c(outl, grep(ss, fl, value=TRUE))
		}
		fl <- outl
	}
	if (!is.null(pat))
		fl <- grep(pat, fl, value=TRUE)
	return(fl)
}

#' Set a tidyverse-friendly conflicts policy
#' Just crashes out; doesn't provide good fixes
#' Should be deprecated, maybe for reportConflicts
#' @export
manageConflicts <- function(){
	options(
		conflicts.policy = list(
			error = TRUE, warn = FALSE, generics.ok = TRUE
			, can.mask = c("base", "methods", "utils"
				, "grDevices", "graphics", "stats"
			)
			, depends.ok = TRUE
		)
		, tidyverse.quiet = TRUE
	)
	invisible(NULL)
}

#' Let tidyverse mask stuff it knows about
#' Warn for other conflicts without crashing
#' @export
reportConflicts <- function(){
	options(
		conflicts.policy = list(
			error = FALSE, warn = FALSE, generics.ok = TRUE
			, can.mask = c("base", "methods", "utils"
				, "grDevices", "graphics", "stats"
			)
			, depends.ok = TRUE
		)
		, tidyverse.quiet = TRUE
	)
	invisible(NULL)
}
