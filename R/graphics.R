
#' Open a graphics file for saving
#' @param ... arguments to pass to grDevices call (via makeGraphics)
#' @param target stem of filename (defaults to the target of the script)
#' @param otype type of file to write (defaults to pdf)
#' @param ext file extension (defaults to otype)
#' @param always open file even if called from interactive model
#' @export
startGraphics <- function(...
	, target = makeArgs()[[1]]
	, otype = "pdf", ext = otype
	, always = FALSE
)
if(always || !interactive()) {
	makeGraphics(..., target, otype, ext)
}

#' Open a graphics file for saving (internal function; exported for legacy reasons; use startGraphics)
#' @param ... arguments to pass to grDevices call
#' @param target stem of filename (defaults to the target of the script)
#' @param otype type of file to write (will use pdf if not specified)
#' @param ext file extension (will use pdf.tmp if not specified)
#' @export
makeGraphics <- function(...
	, target = makeArgs()[[1]]
	, otype = NULL, ext = otype
)
{
	if(is.null(ext)) ext = "pdf.tmp"
	if(is.null(otype)) otype = "pdf"
	fn <- paste0(target, ".", ext)
	grDevices::graphics.off()
	get(otype)(..., file=fn)
}

