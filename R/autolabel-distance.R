
point_point_distance <- function(x, y, series.x, series.y) {
  y_inches_conversion <- 1 / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  x_inches_conversion <- 1 / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]

  distance <- tibble::tibble(xx=series.x,yy=series.y)
  distance$distance <- sqrt(((distance$xx-x)*x_inches_conversion)^2+((distance$yy-y)*y_inches_conversion)^2)

  distance[rank(distance$distance,ties.method="first")==1,]
}

point_line_distance <- function(x, y, series.x, series.y) {
  y_inches_conversion <- 1 / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  x_inches_conversion <- 1 / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]

  distance <- tibble::tibble(x1=series.x[1:(length(series.x)-1)],y1=series.y[1:(length(series.x)-1)],
                             x2=series.x[2:length(series.x)],y2=series.y[2:length(series.x)])

  distance$l2 <- (distance$x1-distance$x2)^2 + (distance$y1-distance$y2)^2

  distance$t <- ((x - distance$x1)*(distance$x2-distance$x1) + (y-distance$y1)*(distance$y2-distance$y1))/distance$l2
  distance$t <- dplyr::case_when(distance$t > 1 ~ 1,
                                 distance$t < 0 ~ 0,
                                 TRUE ~ distance$t)
  distance$xx <- distance$x1 + distance$t*(distance$x2-distance$x1)
  distance$yy <- distance$y1 + distance$t*(distance$y2-distance$y1)

  distance$distance <- sqrt((x - distance$xx)^2 + (y - distance$yy)^2)

  distance <- distance[c("xx", "yy", "distance")]
  distance[rank(distance$distance,ties.method="first")==1,]
}

point_bar_distance_ <- function(x, y, series.x, y1, y2) {
  y_inches_conversion <- 1 / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  x_inches_conversion <- 1 / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]
  distance <- tibble::tibble(x=series.x,
                             y1=y1,
                             y2=y2)

  distance$l2 <- (distance$y1-distance$y2)^2

  distance$t <- ((y-distance$y1)*(distance$y2-distance$y1))/distance$l2
  distance$t <- dplyr::case_when(distance$t > 1 ~ 1,
                                 distance$t < 0 ~ 0,
                                 TRUE ~ distance$t)
  distance$xx <- distance$x
  distance$yy <- distance$y1 + distance$t*(distance$y2-distance$y1)

  distance$distance <- sqrt((x - distance$xx)^2 + (y - distance$yy)^2)

  distance <- distance[c("xx", "yy", "distance")]
  distance[rank(distance$distance,ties.method="first")==1,]
}

point_bar_distance <- function(x, y, series.x, series.y, data, bars, bars.stacked) {
  if (!bars.stacked) {
    return(point_bar_distance_(x, y, series.x, rep(0, length(series.y)), series.y))
  } else {
    bardata <- t(as.matrix(data[bars]))
    row_n <- which(sapply(1:nrow(bardata), function(i) identical(bardata[i,], series.y)))
    if (row_n == 1) {
      return(point_bar_distance_(x, y, series.x, rep(0, length(series.y)), series.y))
    } else {
      bardata <- bardata[1:row_n,]
      bardata_p <- bardata
      bardata_n <- bardata
      bardata_p[bardata <= 0] <- 0
      bardata_n[bardata > 0] <- 0
      if (row_n > 2) {
        y1 <- colSums(bardata_p[1:row_n-1,])
      } else {
        y1 <- bardata_p[1,]
      }
      y2 <- colSums(bardata_p[1:row_n,])
      distance <- point_bar_distance_(x, y, series.x, y1, y2)
      if (row_n > 2) {
        y1 <- colSums(bardata_n[1:row_n-1,])
      } else {
        y1 <- bardata_n[1,]
      }
      y2 <- colSums(bardata_n[1:row_n,])
      distance <- cbind(distance, point_bar_distance_(x, y, series.x, y1, y2))
      return(distance[rank(distance$distance,ties.method="first")==1,])
    }
  }

}

get_distance_series_type <- function(x, y, series.x, series.y, series_type, data, bars, bars.stacked) {
  if (series_type == "line") {
    return(point_line_distance(x, y, series.x, series.y))
  } else if (series_type == "point") {
    return(point_point_distance(x, y, series.x, series.y))
  } else if (series_type == "bar") {
    return(point_bar_distance(x, y, series.x, series.y, data, bars, bars.stacked))
  } else {
    stop("Unknown series type.")
  }
}

get_distance <- function(a, b, data, series.x, series.y, thisseries, otherseries, series_types, bars, bars.stacked) {
  result <- get_distance_series_type(a,b,series.x,series.y, series_types[[thisseries]], data, bars, bars.stacked)
  los <- lineofsight(result$xx, result$yy, a, b, series.x, data, otherseries)
  next_closest <-
    sapply(otherseries, function(series)
      get_distance_series_type(a, b, series.x, data[[series]], series_types[[series]], data, bars, bars.stacked)$distance)

  if (length(otherseries) == 0) {
    # TODO: Find a way of measuring distance to series on RHS panels
    next_closest <- Inf
  }
  return(list(distance = result$distance, los = los, next_closest = min(next_closest)))
}
