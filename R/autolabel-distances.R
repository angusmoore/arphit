pointlinedistance <- function(x, y, x1, y1, x2, y2) {
  A <- x - x1
  B <- y - y1
  C <- x2 - x1
  D <- y2 - y1

  dot <- A * C + B * D
  len_sq <- C * C + D * D
  param <- -1
  if (len_sq != 0) {
    param = dot / len_sq
  }

  if (param < 0) {
    xx <- x1
    yy <- y1
  } else if (param > 1) {
    xx <- x2
    yy <- y2
  } else {
    xx <- x1 + param * C
    yy <- y1 + param * D
  }
  return (list(dist = distanceininches(x, y, xx, yy), x = xx, y = yy))
}

lineofsight.point2point <- function(x, y, a, b, block.x, block.y) {
  for (i in 1:(length(block.x)-1)) {
    if (linesegment.intersect(x, y, a, b, block.x[i], block.y[i], block.x[i+1], block.y[i+1])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

lineofsight <- function(x, y, a, b, data, serieslist, thisseries) {
  block.x <- stats::time(data)
  los <- TRUE
  for (s in serieslist) {
    if (s != thisseries) {
      block.y <- data[, s]
      los <- los && lineofsight.point2point(x, y, a, b, block.x, block.y)
    }
  }
  return(los)
}

series.distance <- function(a, b, data, serieslist, thisseries, requirelos) {
  series.x <- stats::time(data) + 1.0/(2*stats::frequency(data))
  series.y <- data[, thisseries]
  distance <- Inf
  px <- NULL
  py <- NULL
  for (i in 1:(length(series.x)-1)) {
    tmp <- pointlinedistance(a, b, series.x[i], series.y[i], series.x[i+1], series.y[i+1])
    if (!requirelos || lineofsight(series.x[i], series.y[i], a, b, data, serieslist, thisseries) || lineofsight(series.x[i+1], series.y[i+1], a, b, data, serieslist, thisseries)) {
      if (tmp$dist < distance) {
        ininches <- distanceininches(a, b, tmp$x, tmp$y)
        distance <- tmp$dist
        px <- tmp$x
        py <- tmp$y
      }
    }
  }
  return(list(distance = distance, x = px, y = py))
}
