
#' Open a graphics file for saving
#' @param target stem of filename (defaults to the target of the script)
#' @param otype type of file to write (defaults to pdf)
#' @param ext file extension (defaults to otype)
#' @param always open file even if called from interactive mode
#' @param desc text to replace Rout in default target
#' @param ... arguments to pass to grDevices call (via makeGraphics)
#' @export
startGraphics <- function(target = makeArgs()[[1]]
	, otype = "pdf", ext = otype
	, always = FALSE
	, desc = NULL
	, ...
)
if(always || !interactive()) {
	makeGraphics(target=target, otype=otype, ext=ext, desc=desc, ...)
}

#' Open a graphics file for saving (internal function; exported for legacy reasons; use startGraphics)
#' @param target stem of filename (defaults to the target of the script)
#' @param otype type of file to write (will use pdf if not specified)
#' @param ext file extension (will use pdf.tmp if not specified)
#' @param desc text to replace Rout in default target
#' @param ... arguments to pass to grDevices call
#' @export
makeGraphics <- function(target = makeArgs()[[1]]
	, otype = NULL, ext = otype
	, desc = NULL
	, ...
)
{
	if(is.null(ext)) ext = "pdf.tmp"
	if(is.null(otype)) otype = "pdf"
	fn <- paste0(target, ".", ext)
	if (!is.null(desc)) fn <- sub("Rout", desc, fn)
	grDevices::graphics.off()
	get(otype)(..., file=fn)
}

#' save a ggplot object with a name derived from the target name
#' this is good because ggplot objects don't always play well with print
#' and because just it's good to attach mnemonic names when you have a lot of files
#' @param g graphical object to print
#' @param target stem of filename (defaults to the target of the script)
#' @param ext file extension (will use pdf if not specified)
#' @param desc text to replace Rout in default target (ggp by default)
#' @param ... arguments to pass to ggsave call
#' @export
saveGG <- function(g
	, target = makeArgs()[[1]]
	, ext = "pdf"
	, desc = "ggp"
	, ...
)
{
	if(is.null(ext)) ext = "pdf"
	fn <- paste0(target, ".", ext)
	fn <- sub("Rout", desc, fn)
	ggplot2::ggsave(fn, plot=g, ...)
}

#' saveGG and also print to stdout(for quick reference)
#' @param g graphical object to print and save
#' @param target stem of filename (defaults to the target of the script)
#' @param ext file extension (will use pdf if not specified)
#' @param desc text to replace Rout in default target (ggp by default)
#' @param ... arguments to pass to ggsave call
#' @export
teeGG <- function(g
	, target = makeArgs()[[1]]
	, ext = "pdf"
	, desc = "ggp"
	, print_title = desc
	, ...
)
{
	pg <- ifelse(is.null(print_title), g, g+ggtitle(print_title))
	print(g)
	if(is.null(ext)) ext = "pdf"
	fn <- paste0(target, ".", ext)
	fn <- sub("Rout", desc, fn)
	ggplot2::ggsave(fn, plot=g, ...)
}
