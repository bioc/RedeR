.onAttach <- function(libname = find.package("RedeR"),
  pkgname = "RedeR"){
  version <- packageVersion("RedeR")
  msg <- paste("***This is RedeR ",version,"! ",
  "For a quick start, type vignette('RedeR').\n",sep="")
  packageStartupMessage(msg, appendLF = FALSE)
}

.onLoad <- function(libname = find.package("RedeR"),
  pkgname = "RedeR"){
  opt <- list()
  opt$port <- RedPort()
  opt$unit <- "point"
  options( RedeR = opt )
}
