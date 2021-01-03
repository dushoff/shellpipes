

#' Get a targetname
#' @param ext file extension for output
#' @param suffix file extension of provided name (.Rout by default)
#' @param fn provided file name (first of commandArgs by default)
#' @value a filename to write something to
#' @export
targetname <- function(ext="", suffix="\\.Rout", fn = makeArgs()[[1]]){
	return(sub(suffix, ext, fn))
}

#' Try to find a single file matching a given description (usually for reading)
#' @param pat is a pattern for matching
#' @param fl is the list of filenames to search (makeArgs by default)
#' @param exts is a list of allowed extensions
#' @export
matchFile <-  function(pat, fl = makeArgs(), exts=NULL){
	f <- fileSelect(fl, exts, pat)
	if (length(f) == 0) stop("No match for ", pat, " in ", fl)
	if (length(f) > 1) stop("More than one match for ", pat, " in ", fl)
	return(f)
}

#' Not exported: makeArgs
#' A service function to get the make arguments
#' when R was called interactively, these come from a variable called callArgs
#' otherwise parsed from the command line
#' @param call pass callArgs directly
makeArgs <- function(call=callArgs){
	if(interactive()){
		if (!exists("callArgs"))
			stop("Define callArgs to use makeR files; see .args file?")
		return(strsplit(call, " ")[[1]])
	}
	return(commandArgs(TRUE))
}

#' Not exported: fileSelect
#' Select a list of filenames from a list by extension
#' … and optionally by pattern as well
#' not currently exported; used by other file selectors
#' @param fl filelist (makeArgs by default)
#' @param exts a list of extensions to match (. is added here)
#' @param pat a pattern to filter by
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

