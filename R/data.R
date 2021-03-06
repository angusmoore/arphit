check_series_has_obs_is_finite <- function(y, name) {
  if (length(y) == 0) {
    stop(paste0("Series ", name, " has no observations."), call. = FALSE)
  }
  if (any(is.infinite(y))) {
    stop(paste0("Series ", name, " contains non-finite values."), call. = FALSE)
  }
}

is.acceptable.data <- function(data) {
  is.data.frame(data) || stats::is.ts(data) ||
    zoo::is.zoo(data) || xts::is.xts(data)
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

create_series <- function(name, x, y, geomtype) {
  check_series_has_obs_is_finite(y, name)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  list(name = name,
       x = x,
       y = y,
       geomtype = geomtype,
       attributes = list())
}

series_names <- function(x) {
  sapply(x$series, function(y) y$name)
}

extract_bar_data <- function(data, geomtype = "bar") {
  colours <- c()
  bordercol <- c()
  bardata <- data.frame(agg_xvalues = data$x, stringsAsFactors = FALSE)

  for (i in seq_along(data$series)) {
    s <- data$series[[i]]
    if (s$geomtype == geomtype) {
      colours <- append(colours, s$attributes$col)
      bordercol <- append(bordercol, s$attributes$barcol)
      series_data <- data.frame(agg_xvalues = series_x_values(data, i),
                                y = series_values(data, i),
                                stringsAsFactors = FALSE)
      names(series_data) <- c("agg_xvalues", i)
      if (anyDuplicated(series_data$agg_xvalues)) {
        stop(
          paste0(
            "Series ",
            s$name,
            " invalid. Bar graphs cannot have duplicate entries for x values."
          ),
          call. = FALSE
        )
      }
      bardata <- dplyr::left_join(bardata, series_data, by = "agg_xvalues")
    }
  }
  bardata <- bardata[names(bardata) != "agg_xvalues"]

  append(data,
         list(bars = list(
           bardata = bardata,
           colours = colours,
           bordercol = bordercol
         )))
}

get_bar_data <- function(data) {
  data$bars
}

equal_date_spacing <- function(x, freq) {
  if (freq == 1 / 365 || freq == 1 / (365 / 7)) {
    # special case days and weeks as there isn't a whole number of them /
    # consistently the same number of them in a year
    make_decimal_date(
      seq(
        from = min(x),
        to = max(x),
        by = dplyr::case_when(
          freq == 1 / (365 / 7) ~ "week",
          freq == 1 / 365 ~ "day"
        )
      ),
      freq
    )
  } else {
    seq(from = make_decimal_date(min(x), freq),
        to = make_decimal_date(max(x), freq),
        by = freq)
  }
}

convert_to_plot_bardata <- function(bardata, data) {
  if (data$ts) {
    # Widen if we are missing x values (otherwise the bars are in the
    # wrong spot (#157))
    bardata$x <- data$x # I have to round because of small inaccuracies
    equal_spaced <- equal_date_spacing(data$ts_x, data$freq)
    for (x in equal_spaced) {
      if (!any(abs(bardata$x - x) < 1e-10)) {
        # Use fuzzy comparison because of inaccuracies in constructing the
        # sequence
        bardata <- dplyr::add_row(bardata, x = x)
      }
    }
    bardata <- bardata[order(bardata$x), ]
    bardata <- bardata[names(bardata) != "x"]
  }
  bardata_n <- t(as.matrix(bardata))
  colnames(bardata_n) <- NULL
  bardata_n[is.na(bardata_n)] <- 0 # singletons don't show otherwise (#82)
  # Split into positive and negative (R doesn't stack well across axes)
  bardata_p <- bardata_n
  bardata_n <- bardata_n
  bardata_p[bardata_n <= 0] <- 0
  bardata_n[bardata_n > 0] <- 0
  if (data$ts) {
    list(p = bardata_p, n = bardata_n, x = equal_spaced)
  } else {
    list(p = bardata_p, n = bardata_n, x = get_x_plot_locations(data$x, data))
  }
}
