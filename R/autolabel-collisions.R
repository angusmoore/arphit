linesegment.intersect <- function(x1, y1, x2, y2, a1, b1, a2, b2) {
  if (is.na(x1) || is.na(y1) || is.na(x2) || is.na(y2) || is.na(a1) || is.na(b1) || is.na(a2) || is.na(b2)) {
    stop("huh")
  }
  if (max(x1, x2) < min(a1, a2) || min(x1, x2) > max(a1, a2) || max(y1, y2) < min(b1, b2) || min(y1, y2) > max(b1, b2)) {
    return(FALSE)
  } else {
    return(((a1-x1)*(y2-y1) - (b1-y1)*(x2-x1)) * ((a2-x1)*(y2-y1) - (b2-y1)*(x2-x1)) <= 0 && ((x1-a1)*(b2-b1) - (y1-b1)*(a2-a1)) * ((x2-a1)*(b2-b1) - (y2-b1)*(a2-a1)) <= 0)
  }
}

text.bounding <- function(text, x, y, cex = 1) {
  w <- graphics::strwidth(text, cex = cex, units = "user")
  h <- graphics::strheight(text, cex = cex, units = "user")

  lx <- x - (w / 2) - asunits(c(LABELTEXTPADDING, 0))[1]
  ty <- y + (h / 2) + asunits(c(0, LABELTEXTPADDING))[2]
  rx <- x + (w / 2) + asunits(c(LABELTEXTPADDING, 0))[1]
  by <- y - (h / 2) - asunits(c(0, LABELTEXTPADDING))[2]

  return(list(lx = lx, ty = ty, rx = rx, by = by))
}

text.collision <- function(text, x, y, a1, b1, a2, b2) {
  bound <- text.bounding(text, x, y)
  return((linesegment.intersect(bound$lx, bound$ty, bound$rx, bound$ty, a1, b1, a2, b2) ||
            linesegment.intersect(bound$lx, bound$by, bound$rx, bound$by, a1, b1, a2, b2) ||
            linesegment.intersect(bound$lx, bound$ty, bound$lx, bound$by, a1, b1, a2, b2) ||
            linesegment.intersect(bound$rx, bound$ty, bound$rx, bound$by, a1, b1, a2, b2)))
}

outofbounds <- function(text, x, y, xlim, ylim) {
  bound <- text.bounding(text, x, y)
  return (bound$lx < xlim[1] ||
            bound$rx > xlim[2] ||
            bound$by < ylim[1] ||
            bound$ty > ylim[2])
}

grid.collision <- function(text, x, y, xlim, ylim, ylim_n) {
  ygrid <- seq(from = ylim[1], to = ylim[2], length.out = ylim_n)
  for (yg in ygrid) {
    if (text.collision(text, x, y, xlim[1], yg, xlim[2], yg)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

series.collision <- function(text, x, y, series.x, data, series) {
  series.y <- data[[series]]
  for (i in 1:(length(series.x)-1)) {
    a1 <- series.x[[i]]
    b1 <- series.y[[i]]
    a2 <- series.x[[i+1]]
    b2 <- series.y[[i+1]]
    if (text.collision(text, x, y, a1, b1, a2, b2)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

allseries.collision <- function(text, x, y, series.x, data, serieslist) {
  for (series in serieslist) {
    if (series.collision(text, x, y, series.x, data, series)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

label.collision <- function(text, x, y, labellocations, labelsmap) {
  for (label in names(labellocations)) {
    bounding <- text.bounding(labelsmap[[label]], labellocations[[label]][1], labellocations[[label]][2])
    if ((x <= bounding$rx & x >= bounding$lx & y <= bounding$ty & y >= bounding$by) ||
        text.collision(text, x, y, bounding$lx, bounding$ty, bounding$rx, bounding$ty) ||
        text.collision(text, x, y, bounding$lx, bounding$by, bounding$rx, bounding$by) ||
        text.collision(text, x, y, bounding$lx, bounding$ty, bounding$lx, bounding$by) ||
        text.collision(text, x, y, bounding$rx, bounding$ty, bounding$rx, bounding$by)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

checkcollisions <- function(text, x, y, series.x, data, serieslist, labellocations, labelsmap, xlim, ylim, ylim_n) {
  return(allseries.collision(text, x, y, series.x, data, serieslist) ||
           grid.collision(text, x, y, xlim, ylim, ylim_n) ||
           label.collision(text, x, y, labellocations, labelsmap) ||
           outofbounds(text, x, y, xlim, ylim))
}
