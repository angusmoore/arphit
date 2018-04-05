frequencyof <- function(dates) {
  smallestdiff <- min(diff(dates))
  options <- c(1, 1/4, 1/12, 1/365)
  bestchoice <- (abs(log(smallestdiff)-log(options)) == min(abs(log(smallestdiff) -log(options))))
  return(options[bestchoice])
}

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
  for (p in c("1", "2", "3", "4", "5", "6", "7", "8")) {
    if (!is.null(data[[p]])) {
      if (stats::is.ts(data[[p]])) {
        outx[[p]] <- as.vector(stats::time(data[[p]])) + 1.0/(2*stats::frequency(data[[p]]))
        outx[[paste(p,"ts",sep = "")]] <- TRUE
        if (!is.null(x[[p]])) {
          warning("Cannot supply x variable if your data are a time series. Ignoring x variable.")
        }
      } else {
        if (is.list(x) && !is.null(x[[p]])) {
          if (x[[p]] %in% names(data[[p]])) {
            outx[[p]] <- data[[p]][[x[[p]]]]
            # if is dates, convert to year fractions
            if (lubridate::is.Date(outx[[p]])) {
              outx[[p]] <- lubridate::decimal_date(outx[[p]])
              # shift by half, so that we're between ticks
              outx[[p]] <- outx[[p]] + 1/2*frequencyof(outx[[p]])
              # Add a little helper to tell other functions we have time series data
              outx[[paste0(p, "ts")]] <- TRUE
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
