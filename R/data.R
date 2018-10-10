check_data_has_rows_is_finite <- function(series, data) {
  for (p in names(series)) {
    if (!is.null(data[[p]]) && nrow(data[[p]]) == 0) {
      stop(paste0("Data in panel ", p, " has no rows."))
    }
    for (s in series[[p]]) {
      if (any(is.infinite(data[[p]][[s]]))) {
        stop(paste0("Series ", s, " in panel ", p, " contains non-finite values."))
      }
    }
  }
}

is.acceptable.data <- function(data) {
  return(tibble::is_tibble(data) || is.data.frame(data) || stats::is.ts(data) || zoo::is.zoo(data) || xts::is.xts(data))
}

get_series_names <- function(data, x) {
  series <- list()
  # sanity check first
  if (any(!names(data) %in% c("1", "2", "3", "4", "5", "6", "7", "8"))) {
    stop("Invalid index in data sets. Indexes must correspond to panel numbers between 1 and 8.")
  }

  for (p in c("1", "2", "3", "4", "5", "6", "7", "8")) {
    series[[p]] <- colnames(data[[p]])
    if (!is.null(x[[p]])) {
      series[[p]] <- series[[p]][series[[p]]!=x[[p]]]
    }
  }
  check_data_has_rows_is_finite(series, data)
  return(series)
}

arrange_data <- function(data, x, xvals) {
  for (p in c("1", "2", "3", "4", "5", "6", "7", "8")) {
    # Sort ts data by date, otherwise graphs come out WEIRD!
    if (!is.null(xvals[[paste0(p,"ts")]])) {
      data[[p]] <- dplyr::arrange_(data[[p]], x[[p]])
      xvals[[p]] <- sort(xvals[[p]])
    }
  }
  return(list(data = data, xvals = xvals))
}
