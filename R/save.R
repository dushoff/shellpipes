
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

#' Save a variable to rds
#' @param vname name of variable
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (rds)
#' @export
rdsSave <- function(vname, target = targetname(), ext="rds"){
	saveRDS(vname, file=paste(target, ext, sep="."))
}

#' Save a table to csv
#' @param ... arguments to write_csv (typically, name of variable to save)
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (.Rout.csv)
#' @export
csvSave <- function(..., target = targetname(), ext="Rout.csv"){
	readr::write_csv(file=paste(target, ext, sep="."), ...)
}

#' Save a table to tsv
#' @param ... arguments to write_tsv (typically, name of variable to save)
#' @param target stem of file to save to (defaults to name from call)
#' @param ext file extension (.Rout.tsv)
#' @export
tsvSave <- function(..., target = targetname(), ext="Rout.tsv"){
	readr::write_tsv(file=paste(target, ext, sep="."), ...)
}
