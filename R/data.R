sanitycheckdata <- function(series, data) {
  for (p in names(series)) {
    for (s in series[[p]]) {
      if (!s %in% colnames(data[[p]])) {
        stop(paste("Series ", s, " is not in your dataset for panel ", p , ".", sep = ""))
      }
    }
  }
}

is.acceptable.data <- function(data) {
  return(tibble::is_tibble(data) || is.data.frame(data) || is.ts(data))
}

handledata <- function(series, data, x) {
  out <- list(data = list(), series = list())
  if (is.acceptable.data(data)) {
    # We do not have different data for each panel
    # First test if we have specified series. If not, just apply them all on 1
    if (is.null(series)) {
      out$series <- list("1" = colnames(data))
      # strip out the x var if it exists
      if (!is.null(x)) {
        if (is.list(x)) {
          stop("Cannot specify different x variables for different panels if you supply only a single dataset. Supply only one x variables.")
        } else {
          if (!(x %in% out$series[["1"]])) {
            stop(paste("Your supplied x variable", x, "is not in your data"))
          } else if (!is.ts(data)) {
            out$series[["1"]] <- out$series[["1"]][out$series[["1"]]!=x]
          }
        }
      }
    } else {
      if (is.list(series)) {
        out$series <- series
      } else {
        stop("The value you passed in for series is not a list and it needs to be.")
      }
    }

        # Now apply the parent data to all panels with non null series
    for (p in c("1", "2", "3", "4")) {
      if (!is.null(out$series[[p]])) {
        out$data[[p]] <- data
      }
    }
  } else if (is.list(data)) {
    # have separate data for each panel
    # sanity check first
    for (p in c("1", "2", "3", "4")) {
      if (!is.null(data[[p]]) && !is.acceptable.data(data[[p]])) {
        stop(paste("The data you passed in for panel ", p, " is not a data.frame, tibble or ts.", sep = ""))
      }
    }
    out$data <- data
    for (p in c("1", "2", "3", "4")) {
      if (is.null(series[[p]])) {
        out$series[[p]] <- colnames(out$data[[p]])
      } else {
        out$series[[p]] <- series[[p]]
      }
      if (!is.null(x[[p]]) && !is.ts(out$data[[p]])) {
        out$series[[p]] <- out$series[[p]][out$series[[p]]!=x[[p]]]
      }
    }
  } else {
    stop(paste("The data you passed in is not a data.frame, tibble or ts.", sep = ""))
  }

  sanitycheckdata(out$series, out$data)

  return(out)
}
