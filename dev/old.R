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

#### Saving
