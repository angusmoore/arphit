addarrows <- function(series.x, data, panels, labelsmap, labellocations, colors, panelnumber) {
  arrows <- list()
  serieslist <- names(labellocations)
  for (thisseries in serieslist) {
    d <- series.distance(labellocations[[thisseries]][1], labellocations[[thisseries]][2], series.x, data, serieslist, thisseries, TRUE)
    if (d$dist > MAXLABELDISTANCE) {
      # Either is a long way away from LOS point, or has no LOS. Put an arrow on
      # Find closest point without LOS
      p <- series.distance(labellocations[[thisseries]][1], labellocations[[thisseries]][2], series.x, data, serieslist, thisseries, FALSE)
      head.x <- p$x
      head.y <- p$y
      bounding <- text.bounding(labelsmap[[thisseries]], labellocations[[thisseries]][1], labellocations[[thisseries]][2])

      # Pick which of the four tail locations we want
      xdist <- distanceininches(labellocations[[thisseries]][1], 0, head.x, 0)
      ydist <- distanceininches(0, labellocations[[thisseries]][2], 0, head.y)
      if (head.y > bounding$ty && ydist > xdist) {
        # Top
        tail.x <- labellocations[[thisseries]][1]
        tail.y <- bounding$ty
      } else if (head.x < bounding$lx && xdist > ydist) {
        # Left
        tail.x <- bounding$lx
        tail.y <- labellocations[[thisseries]][2]
      } else if (head.y < bounding$ty && ydist > xdist) {
        # Bottom
        tail.x <- labellocations[[thisseries]][1]
        tail.y <- bounding$by
      } else if (head.x > bounding$rx && xdist > ydist) {
        # Right
        tail.x <- bounding$rx
        tail.y <- labellocations[[thisseries]][2]
      } else {
        # shouldn't need an arrow if this happens...
        message(paste("Unable to validly place arrow for", thisseries))
        next
      }

      newarrow <- list(tail.x = tail.x, tail.y = tail.y, head.x = head.x, head.y = head.y, color = colors[[thisseries]], panel = panelnumber)
      arrows <- append(arrows, list(newarrow))
    }
  }
  return(arrows)
}
