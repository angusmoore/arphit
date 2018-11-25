
point_point_distance <- function(x, y, series.x, series.y, inches_conversion) {
  distance <- sqrt(((series.x-x)*inches_conversion$x)^2+((series.y-y)*inches_conversion$y)^2)
  distance[is.na(distance)] <- Inf

  best <- distance == min(distance)
  return(list(xx=series.x[best][1],yy=series.y[best][1],distance=distance[best][1]))
}

point_line_distance <- function(x, y, series.x, series.y, inches_conversion) {
  x1 <- series.x[1:(length(series.x)-1)]
  y1 <- series.y[1:(length(series.x)-1)]
  x2 <- series.x[2:length(series.x)]
  y2 <- series.y[2:length(series.x)]

  l2 <- (x1-x2)^2 + (y1-y2)^2

  t <- ((x - x1)*(x2-x1) + (y-y1)*(y2-y1))/l2
  t[t<0] <- 0
  t[t>1] <- 1

  xx <- x1 + t*(x2-x1)
  yy <- y1 + t*(y2-y1)

  distance <- sqrt(((xx-x)*inches_conversion$x)^2+((yy-y)*inches_conversion$y)^2)
  distance[is.na(distance)] <- Inf

  best <- distance == min(distance)
  return(list(xx=xx[best][1],yy=yy[best][1],distance=distance[best][1]))
}

point_bar_distance_ <- function(x, y, series.x, y1, y2, inches_conversion) {
  l2 <- (y1-y2)^2

  t <- (y-y1)*(y2-y1)/l2
  t[t<0] <- 0
  t[t>1] <- 1

  xx <- series.x
  yy <- y1 + t*(y2-y1)

  distance <- sqrt(((xx-x)*inches_conversion$x)^2+((yy-y)*inches_conversion$y)^2)
  distance[is.na(distance)] <- Inf
  best <- distance == min(distance)
  return(list(xx=xx[best][1],yy=yy[best][1],distance=distance[best][1]))
}

point_bar_distance <- function(x, y, series.x, series.y, data, bars, bars.stacked, inches_conversion) {
  if (!bars.stacked) {
    return(point_bar_distance_(x, y, series.x, rep(0, length(series.y)), series.y, inches_conversion))
  } else {
    bardata <- t(as.matrix(data[bars]))
    row_n <- which(sapply(1:nrow(bardata), function(i) identical(bardata[i,], series.y)))
    if (row_n == 1) {
      return(point_bar_distance_(x, y, series.x, rep(0, length(series.y)), series.y, inches_conversion))
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
      distance_below <- point_bar_distance_(x, y, series.x, y1, y2, inches_conversion)
      if (row_n > 2) {
        y1 <- colSums(bardata_n[1:row_n-1,])
      } else {
        y1 <- bardata_n[1,]
      }
      y2 <- colSums(bardata_n[1:row_n,])
      distance_above <- point_bar_distance_(x, y, series.x, y1, y2, inches_conversion)
      if (distance_below$distance < distance_above$distance) {
        return(distance_below)
      } else {
        return(distance_above)
      }
    }
  }

}

get_distance_series_type <- function(x, y, series.x, series.y, series_type, data, bars, bars.stacked, inches_conversion) {
  if (series_type == "line") {
    return(point_line_distance(x, y, series.x, series.y, inches_conversion))
  } else if (series_type == "point") {
    return(point_point_distance(x, y, series.x, series.y, inches_conversion))
  } else if (series_type == "bar") {
    return(point_bar_distance(x, y, series.x, series.y, data, bars, bars.stacked, inches_conversion))
  }
}

get_distance <- function(a, b, data, series.x, series.y, thisseries, otherseries, series_types, bars, bars.stacked, los_mask, xlim, ylim, inches_conversion) {
  result <- get_distance_series_type(a,b,series.x,series.y, series_types[[thisseries]], data, bars, bars.stacked, inches_conversion)
  los <-
    lineofsight(
      result$xx,
      result$yy,
      a,
      b,
      los_mask,
      xlim,
      ylim
    )
  next_closest <-
    sapply(otherseries, function(series)
      get_distance_series_type(a, b, series.x, data[[series]], series_types[[series]], data, bars, bars.stacked, inches_conversion)$distance)

  if (length(otherseries) == 0) {
    # TODO: Find a way of measuring distance to series on RHS panels
    next_closest <- Inf
  }
  return(list(distance = result$distance, los = los, next_closest = min(next_closest), xx = result$xx, yy = result$yy))
}
