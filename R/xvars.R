frequencyof <- function(dates) {
  # Remove duplicate dates first
  dates <- unique(dates)
  for (frequency in c(1, 1/2, 1/4, 1/12, 1/(365/7), 1/365)) {
    if (!any(duplicated(make_decimal_date(dates, frequency)))) {
      return(frequency)
    }
  }
  return(NULL)
}

days_in_year <- function(date) {
  unname(lubridate::days_in_month(lubridate::make_date(lubridate::year(date), 2, 1))) + 337
}

make_decimal_date <- function(date, frequency) {
  if (is.null(frequency)) {
    return(lubridate::decimal_date(date))
  } else if (frequency == 1) {
    return(lubridate::year(date) + 0.5)
  } else if (frequency == 1/2) {
    return(lubridate::year(date) + (lubridate::semester(date) - 0.5)/2)
  } else if (frequency == 1/4) {
    return(lubridate::year(date) + (lubridate::quarter(date) - 0.5)/4)
  } else if (frequency == 1/12) {
    return(lubridate::year(date) + (lubridate::month(date) - 0.5)/12)
  } else if (frequency == 1/(365/7)) {
    return(lubridate::year(date) + (lubridate::week(date) - 0.5)/(365/7))
  } else if (frequency == 1/365) {
    return(lubridate::year(date) + (lubridate::yday(date) - 0.5)/days_in_year(date))
  }
  stop("Unknown frequency", call. = FALSE)
}


get_x_plot_locations <- function(x, data) {
  if (data$ts || is.scatter(data$x) || is.null(data)) {
    # time series or scatter
    return(x)
  } else if (!is.null(data$x)) {
    # Categorical data, offset by half
    return(match(x, data$x) + 0.5)
  }
}

convert_ts_to_decimal_date <- function(data) {
  for (p in names(data)) {
    # if is dates, convert to year fractions
    if (class(data[[p]]$x) == "yearqtr" || class(data[[p]]$x) == "yearmon") {
      data[[p]]$x <- zoo::as.Date(data[[p]]$x)
    }
    if (lubridate::is.Date(data[[p]]$x) || lubridate::is.POSIXt(data[[p]]$x)) {
      freq <- frequencyof(data[[p]]$x)
      data[[p]]$x <- make_decimal_date(data[[p]]$x, freq)
      for (i in seq_along(data[[p]]$series)) {
        data[[p]]$series[[i]]$x <- make_decimal_date(data[[p]]$series[[i]]$x, freq)
      }
      # Add a little helper to tell other functions we have time series data
      data[[p]]$ts <- TRUE
      data[[p]]$freq <- freq
    } else {
      data[[p]]$ts <- FALSE
    }
  }
  return(data)
}
