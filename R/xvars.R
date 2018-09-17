frequencyof <- function(dates) {
  smallestdiff <- min(abs(diff(dates)))
  options <- c(1, 1/4, 1/12, 1/365)
  bestchoice <- (abs(log(smallestdiff)-log(options)) == min(abs(log(smallestdiff) -log(options))))
  return(options[bestchoice])
}

get_x_values <- function(data, x) {
  outx <- list()
  for (p in names(data)) {
    if (!is.null(x[[p]])) {
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
        stop(paste0("The x variable you supplied for panel ", p, " (" , x[[p]], ") is not a variable in the data you supplied."))
      }
    } else {
      stop(paste0("Have not supplied an x variable for panel ", p, " and cannot guess it because it is not a time series."))
    }
  }
  return(outx)
}
