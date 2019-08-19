point_point_distance <- function(x, y, series_x, series_y, inches_conversion) {
  distance <- sqrt(
                ((series_x - x) * inches_conversion$x) ^ 2 +
                ((series_y - y) * inches_conversion$y) ^ 2
              )
  distance[is.na(distance)] <- Inf

  best <- distance == min(distance)
  list(xx = series_x[best][1],
       yy = series_y[best][1],
       distance = distance[best][1])
}

point_line_distance <- function(x, y, series_x, series_y, inches_conversion) {
  x1 <- series_x[1:(length(series_x) - 1)]
  y1 <- series_y[1:(length(series_y) - 1)]
  x2 <- series_x[2:length(series_x)]
  y2 <- series_y[2:length(series_y)]

  l2 <- (x1 - x2) ^ 2 + (y1 - y2) ^ 2

  t <- ((x - x1) * (x2 - x1) + (y - y1) * (y2 - y1)) / l2
  t[t < 0] <- 0
  t[t > 1] <- 1

  xx <- x1 + t * (x2 - x1)
  yy <- y1 + t * (y2 - y1)

  distance <- sqrt(
    ((xx - x) * inches_conversion$x) ^ 2 + ((yy - y) * inches_conversion$y) ^ 2
  )
  distance[is.na(distance)] <- Inf

  best <- distance == min(distance)

  list(xx = xx[best][1],
       yy = yy[best][1],
       distance = distance[best][1])
}

point_bar_distance_single_bar <- function(x, y, xx, y1, y2, inches_conversion) {
  l2 <- (y1 - y2) ^ 2

  t <- (y - y1) * (y2 - y1) / l2
  t[t < 0] <- 0
  t[t > 1] <- 1

  yy <- y1 + t * (y2 - y1)

  distance <- sqrt(
    ((xx - x) * inches_conversion$x) ^ 2 + ((yy - y) * inches_conversion$y) ^ 2
  )
  distance[is.na(distance)] <- Inf
  best <- distance == min(distance)

  list(xx = xx[best][1], yy = yy[best][1], distance = distance[best][1])
}

point_bar_distance <- function(x, y, series_x, series_y,
                               data, stacked, inches_conversion) {
  if (!stacked) {
    return(
      point_bar_distance_single_bar(
        x, y, series_x, rep(0, length(series_y)), series_y, inches_conversion
      )
    )
  } else {
    barseries <- sapply(data$series,
                        function(s) s$geomtype == "bar",
                        USE.NAMES = FALSE)
    row_n <- which(
      sapply(data$series[barseries],
             function(s) identical(s$y, series_y))
    )

    if (length(row_n) > 1) {
      # only occurs if have two identical bar series. Rare special case.
      row_n <- row_n[1]
    }

    if (row_n == 1) {
      return(
        point_bar_distance_single_bar(
          x, y, series_x, rep(0, length(series_y)), series_y, inches_conversion
        )
      )
    } else {
      # We have to widen the bar data to the full plot x
      series_x <- get_x_plot_locations(data$x, data)
      out <- convert_to_plot_bardata(get_bar_data(data)$bardata, data)
      bardata_p <- out$p
      bardata_n <- out$n
      if (row_n > 2) {
        y1 <- colSums(bardata_p[1:(row_n - 1), ])
      } else {
        y1 <- bardata_p[1, ]
      }
      y2 <- colSums(bardata_p[1:row_n, ])
      distance_below <- point_bar_distance_single_bar(x,
                                                      y,
                                                      series_x,
                                                      y1,
                                                      y2,
                                                      inches_conversion)
      if (row_n > 2) {
        y1 <- colSums(bardata_n[1:(row_n - 1), ])
      } else {
        y1 <- bardata_n[1, ]
      }
      y2 <- colSums(bardata_n[1:row_n, ])
      distance_above <- point_bar_distance_single_bar(x,
                                                      y,
                                                      series_x,
                                                      y1,
                                                      y2,
                                                      inches_conversion)
      if (distance_below$distance < distance_above$distance) {
        return(distance_below)
      } else {
        return(distance_above)
      }
    }
  }
}

point_waterfall_distance <- function(x,
                                     y,
                                     series_x,
                                     series_y,
                                     data,
                                     inches_conversion) {
  data$bars <- NULL # remove the pre-fetched bardata (pre set to create ylimits)
  bars <- extract_bar_data(data, "waterfall")$bars
  barseries <- sapply(data$series,
                      function(s) s$geomtype == "waterfall",
                      USE.NAMES = FALSE)

  row_n <- which(sapply(data$series[barseries],
                        function(s) identical(s$y, series_y)))

  if (length(row_n) > 1) {
    # only occurs if have two identical bar series. Rare special case.
    row_n <- row_n[1]
  }
  out <- convert_to_plot_bardata(bars$bardata, data)
  # We have to widen the bar data to the full plot x
  series_x <- get_x_plot_locations(data$x, data)
  bardata <- out$p + out$n
  y1 <- c() # points at the start of the bar
  y2 <- c() # points at the end of the bar
  for (xi in 1:ncol(bardata)) {
    if (xi > 1 && xi < ncol(bardata)) {
      y_offset <- sum(bardata[, 1:(xi - 1)], na.rm = TRUE)
    } else {
      y_offset <- 0
    }

    if (row_n > 1) {
      preceding_series <- bardata[1:(row_n - 1), xi]
    } else {
      preceding_series <- c()
    }

    if (bardata[row_n, xi] > 0) {
      y1 <- c(y1, y_offset + sum(preceding_series[preceding_series > 0]))
    } else {
      y1 <- c(y1, y_offset + sum(preceding_series[preceding_series < 0]))
    }

    y2 <- c(y2, y1[xi] + unname(bardata[row_n, xi]))
  }
  point_bar_distance_single_bar(x, y, series_x, y1, y2, inches_conversion)
}

get_distance_series_type <- function(x, y, series_x, series_y, series_type,
                                     data, stacked, inches_conversion) {
  if (series_type == "line") {
    point_line_distance(x, y, series_x, series_y, inches_conversion)
  } else if (series_type == "point") {
    point_point_distance(x, y, series_x, series_y, inches_conversion)
  } else if (series_type == "bar") {
    point_bar_distance(x, y, series_x, series_y, data,
                       stacked, inches_conversion)
  } else if (series_type == "step") {
    # line is a good enough approximation
    point_line_distance(x, y, series_x, series_y, inches_conversion)
  } else if (series_type == "waterfall") {
    point_waterfall_distance(x, y, series_x, series_y, data, inches_conversion)
  } else {
    stop(paste0("Autolabeller not supported for layers of type ", series_type),
         .call = FALSE)
  }
}
