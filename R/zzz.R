.onLoad <- function(libname, pkgname) {
  if (exists("ARPHIT_USERCOLORS")) {
    DEFAULTCOLORS <<- ARPHIT_USERCOLORS
  }
}
