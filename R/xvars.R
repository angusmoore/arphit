handlex <- function(data, x) {
  if (is.character(x)) {
    # Just have one for all panels, apply
    tmpx <- x
    x <- list()
    x[["1"]] <- tmpx
    x[["2"]] <- tmpx
    x[["3"]] <- tmpx
    x[["4"]] <- tmpx
  }
  outx <- list()
  for (p in c("1", "2", "3", "4")) {
    if (!is.null(data[[p]])) {
      if (stats::is.ts(data[[p]])) {
        outx[[p]] <- as.vector(stats::time(data[[p]]))
        if (!is.null(x[[p]])) {
          warning("Cannot supply x variable if your data are a time series. Ignoring x variable.")
        }
      } else {
        if (is.list(x) && !is.null(x[[p]])) {
          if (x[[p]] %in% names(data[[p]])) {
            outx[[p]] <- data[[p]][, x[[p]]]
            # if is dates, convert to year fractions
            if (lubridate::is.Date(outx[[p]])) {
              outx[[p]] <- lubridate::decimal_date(outx[[p]])
            }
          } else {
            stop(paste("The x variable you supplied for panel ", p, " (" , x[[p]], ") is not a variable in the data you supplied.", sep = ""))
          }
        } else {
          stop(paste("Have not supplied an x variable for panel ", p, " and cannot guess it because it is not a time series.", sep = ""))
        }
      }
    }
  }
  return(outx)
}
