
## FIXME Case-insensitive extensions
## FIXME csvRead etc. as wrappers
## having readr:: means that readr must be in Imports: in the DESCRIPTION file
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

## Wrapper for legacy makefiles
## By default takes Rout dependencies and assumes rda environments
legacyEnvironments <- function(fl = makeArgs()
	, dep = "Rout", ext="rda")
{
	envl <- fileSelect(fl, dep)
	if(length(envl>1)){
		ss <- paste0(dep, "$")
		envl <- sub(ss, ext, envl[-1])
	}
	loadEnvironments(envl)
	invisible(envl)
}

#### Graphics

startGraphics <- function(...
	, target = makeArgs()[[1]]
	, otype = pdf, ext = otype
	, always = FALSE
)
if(always || !interactive()) {
	makeGraphics(..., target, otype, ext)
}

makeGraphics <- function(...
	, target = makeArgs()[[1]]
	, otype = NULL, ext = otype
)
{
	if(is.null(ext)) ext = "pdf.tmp"
	if(is.null(otype)) otype = "pdf"
	fn <- paste0(target, ".", ext)
	graphics.off()
	get(otype)(..., file=fn)
}

#### Saving

saveEnvironment <- function(target = targetname(), ext="rda"){
	save.image(file=paste(target, ext, sep="."))
}

saveVars <- function(..., target = targetname(), ext="rda"){
	save(file=paste(target, ext, sep="."), ...)
}

rdsSave <- function(vname, target = targetname(), ext="rds"){
	saveRDS(vname, file=paste(target, ext, sep="."))
}

### Output

csvSave <- function(..., target = targetname(), ext="Rout.csv"){
	write.csv(file=paste(target, ext, sep="."), ...)
}
