
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

point_bar_distance <- function(x, y, series.x, series.y, data, bars.stacked, inches_conversion) {
  if (!bars.stacked) {
    return(point_bar_distance_(x, y, series.x, rep(0, length(series.y)), series.y, inches_conversion))
  } else {
    bardata <- get_bar_data(data)$bardata
    row_n <- which(sapply(seq_along(bardata), function(i) identical(bardata[[i]][!is.na(bardata[[i]])], series.y[!is.na(series.y)])))
    if (length(row_n) > 1) row_n <- row_n[1] # only occurs if have two identical bar series. Rare special case.
    # I use !is.na because bardata has been widened to all x observations, while series.y hasn't
    if (row_n == 1) {
      return(point_bar_distance_(x, y, series.x, rep(0, length(series.y)), series.y, inches_conversion))
    } else {
      series.x <- get_x_plot_locations(data$x, data) # We have to widen the bar data to the full plot x
      out <- convert_to_plot_bardata(bardata, data)
      bardata_p <- out$p
      bardata_n <- out$n
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

get_distance_series_type <- function(x, y, series.x, series.y, series_type, data, bars.stacked, inches_conversion) {
  if (series_type == "line") {
    return(point_line_distance(x, y, series.x, series.y, inches_conversion))
  } else if (series_type == "point") {
    return(point_point_distance(x, y, series.x, series.y, inches_conversion))
  } else if (series_type == "bar") {
    return(point_bar_distance(x, y, series.x, series.y, data, bars.stacked, inches_conversion))
  } else if (series_type == "step") {
    return(point_line_distance(x, y, series.x, series.y, inches_conversion)) # good enough approximation
  } else if (series_type == "Waterfall") {
    return(list(xx=NA,yy=NA,distance=NA))
  }
}
