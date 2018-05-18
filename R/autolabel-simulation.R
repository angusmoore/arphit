
distanceininches <- function(x, y, a, b) {
  height <- (y - b) / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  width <- (x - a) / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]
  return(sqrt(height^2 + width^2))
}

asunits <- function(vector) {
  x <- vector[1]
  y <- vector[2]
  yu <- (y) / graphics::par("pin")[2] * (graphics::par("usr")[4] - graphics::par("usr")[3])
  xu <- (x) / graphics::par("pin")[1] * (graphics::par("usr")[2] - graphics::par("usr")[1])
  return(c(xu, yu))
}

coulombrepulsion <- function(a, b, x, y, forceconstant) {
  angle <- atan2((b-y), (a-x))
  distance <- distanceininches(a, b, x, y)
  distance = max(distance, 1e-3) # prevents when two points exactly coincide causing errors
  magnitude <- forceconstant / (distance^2)
  return(c(magnitude*cos(angle), magnitude*sin(angle)))
}

hookesattraction <- function(a, b, x, y, stiffness) {
  angle <- atan2((b-y), (a-x))
  distance <- distanceininches(a, b, x, y)
  magnitude <- -stiffness * distance
  return(c(magnitude*cos(angle), magnitude*sin(angle)))
}

seriesforce <- function(a, b, series.x, series.y) {
  vector <- sum(coulombrepulsion(a, b, series.x, series.y, SERIESREPULSION))
  return(vector)
}
calculate.seriesforces <- function(x, data, labelsmap, label, a, b) {
  vector <- c(0, 0)
  for (s in names(labelsmap)) {
    y <- data[, s]
    vector <- vector + seriesforce(a, b, x, y)
  }
  return(vector)
}

calculate.labelsforce <- function(labellocations, thislabel, a, b) {
  vector <- c(0, 0)
  for (label in names(labellocations)) {
    x <- labellocations[[label]][1]
    y <- labellocations[[label]][2]
    vector <- vector + coulombrepulsion(a, b, x, y, LABELSREPULSION)
  }
  return(vector)
}

calculate.boundingforce <- function(a, b, xlim, ylim) {
  vector <- c(0, 0)
  xpert <- (xlim[2] - xlim[1])*0.1 # attaches the hook inside the bounding box to pull it right in
  ypert <- (ylim[2] - ylim[1])*0.1
  if (a < xlim[1]) {
    vector <- vector + hookesattraction(a, b, xlim[1]+xpert, b, BOUNDINGSTIFFNESS)
  }
  if (a > xlim[2]) {
    vector <- vector + hookesattraction(a, b, xlim[2]-xpert, b, BOUNDINGSTIFFNESS)
  }
  if (b < ylim[1]) {
    vector <- vector + hookesattraction(a, b, a, ylim[1]+ypert, BOUNDINGSTIFFNESS)
  }
  if (b > ylim[2]) {
    vector <- vector + hookesattraction(a, b, a, ylim[2]-ypert, BOUNDINGSTIFFNESS)
  }
  return(vector)
}

calculate.repulsion <- function(l, label, x, data, labelsmap, labellocations, xlim, ylim, ylim_n) {
  a <- l[1]
  b <- l[2]
  vector <- calculate.seriesforces(x, data, labelsmap, label, a, b)
  vector <- vector + calculate.labelsforce(labellocations, label, a, b)
  return(asunits(vector))
}

## Simulation code

movelabel <- function(label, l, v, anchor, x, data, labelsmap, labellocations, xlim, ylim, ylim_n, timestep, dampening) {
  absforce <- 0
  repulsiveforce <- calculate.repulsion(l, label, x, data, labelsmap, labellocations, xlim, ylim, ylim_n)
  anchorforce <- asunits(hookesattraction(l[1], l[2], anchor[1], anchor[2], ANCHORSTIFFNESS))
  bounds <- calculate.boundingforce(l[1], l[2], xlim, ylim)

  # assume mass 1, therefore force = acceleration
  acceleration <- repulsiveforce + anchorforce + bounds

  # Move the label
  l <- l + dampening * v * timestep + 1/2 * acceleration * (timestep ^ 2)

  # Update velocity
  v <- dampening * v + acceleration * timestep
  return(list(l = l, v = v))
}

location.fromanchor <- function(label, anchor, series.x, reduceddata, originaldata, labelsmap, labellocations, xlim, ylim, ylim_n) {
  l <- 0.1 * c(stats::runif(min = xlim[1], max = xlim[2], n = 1), stats::runif(min = ylim[1], max = ylim[2], n = 1)) +
    0.9 * anchor
  v <- c(stats::rnorm(n = 1, sd = (xlim[2] - xlim[1])/10), stats::rnorm(n = 1, sd = (ylim[2] - ylim[1])/10))

  i <- 0
  # DEBUG
  # simcol <- sample(colors(), 1)
  while (abs(distanceininches(v[1], v[2], 0, 0)) > MINMOVEMENTSPEED && i < MAXSIMSTEPS) {
    i <- i + 1
    step <- movelabel(label, l, v, anchor, series.x, reduceddata, labelsmap, labellocations, xlim, ylim, ylim_n, TIMESTEP, DAMPENING)
    l <- step$l
    # DEBUG
    # graphics::points(l[1],l[2],col = simcol)
    v <- step$v
    if (!checkcollisions(labelsmap[[label]], l[1], l[2], series.x, originaldata, names(labelsmap), labellocations, labelsmap, xlim, ylim, ylim_n)) {
      labellocations[[label]] <- l
      return(l)
    }
  }

  return(NULL)
}

sampleanchorpoints <- function(data, labelsmap) {
  ap <- sample(1:nrow(data), length(labelsmap), replace = TRUE)
  names(ap) <- names(labelsmap)
  return(ap)
}

labelsimulation <- function(series.x, data, labelsmap, xlim, ylim, ylim_n) {

  labellocations <- list()

  # Reduce the data to the number of points we want to calculate repulsion from if needed.
  # This is purely for performance when faced with a large number of points.
  if (nrow(data) > REPULSIONPOINTS/2) {
    # need to collapse
    s <- ceiling(nrow(data) / REPULSIONPOINTS)
    reduceddata <- data[seq(1, nrow(data), s), ]
  } else {
    reduceddata <- data
  }

  # Find some sample anchor points
  ap <- sampleanchorpoints(data, labelsmap)

  for (label in names(labelsmap)) {
    anchor <- c(series.x[ap[[label]]], data[ap[[label]], label])
    labellocations[[label]] <- location.fromanchor(label, anchor, series.x, reduceddata, data, labelsmap, labellocations, xlim, ylim, ylim_n)
  }
  return(labellocations)
}
