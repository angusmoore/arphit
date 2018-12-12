frequencyof <- function(dates) {
  dates <- lubridate::decimal_date(dates)
  if (length(dates) > 1) {
    smallestdiff <- min(abs(diff(dates))[abs(diff(dates)) > 0])
    options <- c(1, 1/4, 1/12, 1/(365/7), 1/365)
    bestchoice <- abs(log(smallestdiff) -log(options)) < 0.1
    if (sum(bestchoice) == 1) {
      return(options[bestchoice])
    } else {
      if (smallestdiff > 1) {
        # If frequency is more than a year, just use years
        return(1)
      } else {
        return(NULL)
      }
    }
  } else {
    return(1)
  }
}

make_decimal_date <- function(date, frequency) {
  if (is.null(frequency)) {
    return(lubridate::decimal_date(date))
  } else if (frequency == 1) {
    return(lubridate::year(date) + 0.5)
  } else if (frequency == 1/4) {
    return(lubridate::year(date) + (lubridate::quarter(date) - 0.5)/4)
  } else if (frequency == 1/12) {
    return(lubridate::year(date) + (lubridate::month(date) - 0.5)/12)
  } else if (frequency == 1/(365/7)) {
    return(lubridate::year(date) + (lubridate::week(date) - 0.5)/(365/7))
  } else if (frequency == 1/365) {
    return(lubridate::decimal_date(date) + 0.5/365)
  }
  stop("Unknown frequency")
}

get_x_values <- function(data, x) {
  outx <- list()
  for (p in names(data)) {
    if (!is.null(x[[p]])) {
      if (x[[p]] %in% names(data[[p]])) {
        outx[[p]] <- data[[p]][[x[[p]]]]
        # if is dates, convert to year fractions
        if (lubridate::is.Date(outx[[p]])) {
          freq <- frequencyof(outx[[p]])
          outx[[p]] <- make_decimal_date(outx[[p]], freq)
          # Add a little helper to tell other functions we have time series data
          outx[[paste0(p, "ts")]] <- TRUE
          outx[[paste0(p, "freq")]] <- freq
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
