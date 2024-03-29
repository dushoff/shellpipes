
#' Save the whole environment
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (rda)
#' @export
saveEnvironment <- function(target = targetname(), ext="rda"){
	save.image(file=paste(target, ext, sep="."))
}

#' Save particular variables to rdata
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (rda)
#' @param ... arguments to save (typically, names of variables to save)
#' @export
saveVars <- function(..., target = targetname(), ext="rda"){
	save(file=paste(target, ext, sep="."), ...)
}

#' Save variables to rdata
#' Convenience wrapper for saveEnvironment (all variables) or saveVars (specific variables)
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (rda)
#' @param ... names of variables to save, defaults to NULL which is interpreted as all
#' @export
rdaSave <- function(..., target = targetname(), ext="rda"){
	if (length(list(...))==0)
		saveEnvironment(target, ext)
	else
		saveVars(..., target, ext)
}

#' Serialize with saveRDS using a target stem
#' @param object R object to save
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (rds)
#' @param printSummary logical
#' @export
rdsSave <- function(object, target = targetname(), ext="rds", printSummary=FALSE){
	if (printSummary) print(summary(object))
	saveRDS(object, file=paste(target, ext, sep="."))
}

#' Write to a different kind of file
#' @param ... arguments to sync
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (.Rout.txt)
#' @export
outFile <- function(..., target = targetname(), ext="Rout.txt"){
	sink(file=paste(target, ext, sep="."), ...)
}

#' Save a table to csv
#' @param ... arguments to write_csv (typically, name of variable to save)
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (.Rout.csv)
#' @param writeFun function for writing
#' @export
csvSave <- function(..., target = targetname(), ext="Rout.csv", writeFun=readr::write_csv){
	writeFun(file=paste(target, ext, sep="."), ...)
}

#' Save a table to tsv
#' @param ... arguments to write_tsv (typically, name of variable to save)
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (.Rout.tsv)
#' @export
tsvSave <- function(..., target = targetname(), ext="Rout.tsv"){
	readr::write_tsv(file=paste(target, ext, sep="."), ...)
}
