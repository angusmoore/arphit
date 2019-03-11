check_series_has_obs_is_finite <- function(y, name) {
  if (length(y) == 0) {
    stop(paste0("Series ", name, " has no observations."))
  }
  if (any(is.infinite(y))) {
    stop(paste0("Series ", name, " contains non-finite values."))
  }
}

is.acceptable.data <- function(data) {
  return(tibble::is_tibble(data) || is.data.frame(data) || stats::is.ts(data) || zoo::is.zoo(data) || xts::is.xts(data))
}

new_panel_data <- function(ts = NA, freq = NULL) {
  data <- list(x = c(),
               series = list(),
               ts = ts,
               freq = freq,
               order_mapping = data.frame())
  class(data) <- "panel_data"
  return(data)
}

series_values <- function(data, series) {
  data$series[[series]]$y
}

series_x_values <- function(data, series) {
  data$series[[series]]$x
}

is_empty <- function(panel) {
  length(panel$series) == 0
}

create_series <- function(name, x, y, bar) {
  check_series_has_obs_is_finite(y, name)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  list(name = name,
       x = x,
       y = y,
       bar = bar,
       attributes = list())
}

series_names <- function(x) {
  sapply(x$series, function(y) y$name)
}

get_bar_data <- function(data) {
  colors <- c()
  bordercol <- c()
  bardata <- data.frame(x = data$x, stringsAsFactors = FALSE)

  for (i in seq_along(data$series)) {
    s <- data$series[[i]]
    if (s$bar) {
      colors <- append(colors, s$attributes$col)
      bordercol <- append(bordercol, s$attributes$barcol)
      series_data <- data.frame(x = series_x_values(data, i), y = series_values(data, i), stringsAsFactors = FALSE)
      names(series_data) <- c("x", i)
      if (anyDuplicated(series_data$x)) {
        stop(paste0("Series ", s$name, " invalid. Bar graphs cannot have duplicate entries for x values."))
      }
      bardata <- dplyr::left_join(bardata, series_data, by = "x")
    }
  }
  bardata <- dplyr::select_(bardata, "-x")
  return(list(bardata=bardata, colors=colors, bordercol=bordercol))
}

convert_to_plot_bardata <- function(bardata, data) {
  if (data$ts) {
    # Widen if we are missing x values (otherwise the bars are in the wrong spot (#157))
    bardata$x <- data$x # I have to round because of small inaccuracies
    equal_spaced <- seq(from = min(bardata$x), to = max(bardata$x), by = data$freq)
    for (x in equal_spaced) {
      if (!any(abs(bardata$x - x) < 1e-10)) {
        # Use fuzzy comparison because of inaccuracies in constructing the sequence
        bardata <- dplyr::add_row(bardata, x = x)
      }
    }
    bardata <- dplyr::arrange_(bardata, "x")
    bardata <- dplyr::select_(bardata, "-x")
  }
  bardata_n <- t(as.matrix(bardata))
  bardata_n[is.na(bardata_n)] <- 0 # singletons don't show otherwise (#82)
  # Split into positive and negative (R doesn't stack well across axes)
  bardata_p <- bardata_n
  bardata_n <- bardata_n
  bardata_p[bardata_n <= 0] <- 0
  bardata_n[bardata_n > 0] <- 0
  return(list(p = bardata_p, n = bardata_n))
}
