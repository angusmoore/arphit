frequencyof <- function(dates) {
  # Remove duplicate dates first
  dates <- unique(dates)
  for (frequency in c(1, 1 / 2, 1 / 4, 1 / 12, 1 / (365 / 7), 1 / 365)) {
    if (!any(duplicated(make_decimal_date(dates, frequency)))) {
      return(frequency)
    }
  }

  NULL
}

days_in_year <- function(date) {
  unname(
    lubridate::days_in_month(
      lubridate::make_date(
        lubridate::year(date),
        2,
        1)
      )
    ) + 337
}

weeks_in_year <- function(dates) {
  # What this is doing:
  # weeks are a mess. There is not a whole number of them in a year. As a
  # result, the number of 'weekly' observations in a given year depends on which
  # _day_ is being used to index the week. For instance, if your 'week' starts
  # with 2000-01-01, you'll get 53 'weekly' observations (ending with
  # 2000-12-30), but if it starts 2000-01-04 instead, the 53rd 'week' is
  # 2000-10-02. This is a bug in the calendar. Fixing it seems hard. Caesar
  # should be ashamed.
  dplyr::if_else(
    lubridate::year(dates + lubridate::weeks(53 - lubridate::week(dates))) ==
      lubridate::year(dates),
    53,
    52
  )
}

make_decimal_date <- function(date, frequency) {
  if (is.null(frequency)) {
    lubridate::decimal_date(date)
  } else if (frequency == 1) {
    lubridate::year(date) + 0.5
  } else if (frequency == 1 / 2) {
    lubridate::year(date) + (lubridate::semester(date) - 0.5) / 2
  } else if (frequency == 1 / 4) {
    lubridate::year(date) + (lubridate::quarter(date) - 0.5) / 4
  } else if (frequency == 1 / 12) {
    lubridate::year(date) + (lubridate::month(date) - 0.5) / 12
  } else if (frequency == 1 / (365 / 7)) {
    lubridate::year(date) + (lubridate::week(date) - 0.5) / weeks_in_year(date)
  } else if (frequency == 1 / 365) {
    lubridate::year(date) + (lubridate::yday(date) - 0.5) / days_in_year(date)
  } else {
    stop("Unknown frequency", call. = FALSE)
  }
}


get_x_plot_locations <- function(x, data) {
  if (data$ts || is.scatter(data$x) || is.null(data)) {
    # time series or scatter
    x
  } else if (!is.null(data$x)) {
    # Categorical data, offset by half
    match(x, data$x) + 0.5
  }
}

convert_ts_to_decimal_date <- function(data) {
  for (p in names(data)) {
    # if is dates, convert to year fractions
    if ("yearqtr" %in% class(data[[p]]$x) ||
        "yearmon" %in% class(data[[p]]$x)) {
      data[[p]]$x <- zoo::as.Date(data[[p]]$x)
    }
    if (lubridate::is.Date(data[[p]]$x) || lubridate::is.POSIXt(data[[p]]$x)) {
      freq <- frequencyof(data[[p]]$x)
      data[[p]]$ts_x <- data[[p]]$x
      data[[p]]$x <- make_decimal_date(data[[p]]$x, freq)
      for (i in seq_along(data[[p]]$series)) {
        data[[p]]$series[[i]]$x <- make_decimal_date(data[[p]]$series[[i]]$x,
                                                     freq)
      }
      # Add a little helper to tell other functions we have time series data
      data[[p]]$ts <- TRUE
      data[[p]]$freq <- freq
    } else {
      data[[p]]$ts <- FALSE
    }
  }

  data
}
