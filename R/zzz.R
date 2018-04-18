.onLoad <- function(libname, packagename) {
  if (exists("ARPHIT_USERCOLORS")) {
    DEFAULTCOLORS <<- ARPHIT_USERCOLORS
  }
}
