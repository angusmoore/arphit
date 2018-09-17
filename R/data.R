sanitycheckdata <- function(series, data) {
  for (p in names(series)) {
    if (!is.null(data[[p]]) && nrow(data[[p]]) == 0) {
      stop(paste0("Data in panel ", p, " has no rows."))
    }
    for (s in series[[p]]) {
      if (!s %in% colnames(data[[p]])) {
        stop(paste("Series ", s, " is not in your dataset for panel ", p , ".", sep = ""))
      } else {
        y <- data[[p]][[s]]
        if (any(is.infinite(y))) {
          stop(paste0("Series ", s, " in panel ", p, " contains non-finite values."))
        }
      }
    }
  }
}

is.acceptable.data <- function(data) {
  return(tibble::is_tibble(data) || is.data.frame(data) || stats::is.ts(data))
}

handledata <- function(series, data, x) {
  out <- list(data = list(), series = list())
  # sanity check first
  if (any(!names(data) %in% c("1", "2", "3", "4", "5", "6", "7", "8"))) {
    stop("Invalid index in data sets. Indexes must correspond to panel numbers between 1 and 8.")
  }
  for (p in c("1", "2", "3", "4", "5", "6", "7", "8")) {
    if (!is.null(data[[p]]) && (!tibble::is_tibble(data[[p]])) && !is.data.frame(data[[p]])) {
      stop(paste("The data you passed in for panel ", p, " is not a tibble or data.frame.", sep = ""))
    }
  }
  out$data <- data
  for (p in c("1", "2", "3", "4", "5", "6", "7", "8")) {
    if (is.list(x)) {
      tmpx <- x[[p]]
    } else {
      tmpx <- x
    }

    if (is.null(series[[p]])) {
      out$series[[p]] <- colnames(out$data[[p]])
    } else {
      out$series[[p]] <- series[[p]]
    }
    if (!is.null(tmpx)) {
      out$series[[p]] <- out$series[[p]][out$series[[p]]!=tmpx]
    }
  }
  sanitycheckdata(out$series, out$data)
  return(out)
}

arrange_data <- function(data, x, xvals) {
  for (p in c("1", "2", "3", "4", "5", "6", "7", "8")) {
    if (!is.null(xvals[[paste0(p,"ts")]])) {
      data[[p]] <- dplyr::arrange_(data[[p]], x[[p]])
      xvals[[p]] <- sort(xvals[[p]])
    }
  }
  return(list(data = data, xvals = xvals))
}
