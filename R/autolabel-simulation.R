
distanceininches <- function(x, y, serie.x, serie.y) {
  height <- (y - serie.y) / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  width <- (x - serie.x) / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]
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
  vector <- c(0, 0)
  for (i in 1:(length(series.x)-1)) {
    x1 <- series.x[[i]]
    y1 <- series.y[[i]]
    x2 <- series.x[[i+1]]
    y2 <- series.y[[i+1]]
    n <- ceiling(50 / length(series.x))

    xseq <- seq(from = x1, to = x2, length.out = n)
    yseq <- seq(from = y1, to = y2, length.out = n)
    for (i in 1:n) {
      x <- xseq[i]
      y <- yseq[i]
      vector <- vector + coulombrepulsion(a, b, x, y, SERIESREPULSION)
    }
  }
  return(vector)
}

calculate.seriesforces <- function(data, labelsmap, label, a, b) {
  vector <- c(0, 0)
  for (s in names(labelsmap)) {
    x <- stats::time(data) + 1.0/(2*stats::frequency(data))
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

calculate.repulsion <- function(l, label, data, labelsmap, labellocations, xlim, ylim, ylim_n) {
  a <- l[1]
  b <- l[2]
  vector <- calculate.seriesforces(data, labelsmap, label, a, b)
  vector <- vector + calculate.labelsforce(labellocations, label, a, b)
  vector <- vector + calculate.boundingforce(a, b, xlim, ylim)
  return(asunits(vector))
}

## Simulation code

movelabel <- function(label, l, v, anchor, data, labelsmap, labellocations, xlim, ylim, ylim_n, timestep, dampening) {
  absforce <- 0
  repulsiveforce <- calculate.repulsion(l, label, data, labelsmap, labellocations, xlim, ylim, ylim_n)
  attractiveforce <- asunits(hookesattraction(l[1], l[2], anchor[1], anchor[2], ANCHORSTIFFNESS))
  # assume mass 1, therefore force = acceleration
  acceleration <- repulsiveforce + attractiveforce

  # Move the label
  l <- l + dampening * v * timestep + 1/2 * acceleration * (timestep ^ 2)

  # Update velocity
  v <- dampening * v + acceleration * timestep
  return(list(l = l, v = v))
}

location.fromanchor <- function(label, anchor, data, labelsmap, labellocations, xlim, ylim, ylim_n) {
  l <- c(stats::runif(min = xlim[1], max = xlim[2], n = 1), stats::runif(min = ylim[1], max = ylim[2], n = 1))
  v <- c(stats::rnorm(n = 1, sd = (xlim[2] - xlim[1])/10), stats::rnorm(n = 1, sd = (ylim[2] - ylim[1])/10))

  i <- 0
  while (abs(distanceininches(v[1], v[2], 0, 0)) > MINMOVEMENTSPEED && i < MAXSIMSTEPS) {
    i <- i + 1
    step <- movelabel(label, l, v, anchor, data, labelsmap, labellocations, xlim, ylim, ylim_n, TIMESTEP, DAMPENING)
    l <- step$l
    v <- step$v
    if (!checkcollisions(labelsmap[[label]], l[1], l[2], data, names(labelsmap), labellocations, labelsmap, xlim, ylim, ylim_n)) {
      labellocations[[label]] <- l
      return(l)
    }
  }

  return(NULL)
}

sampleanchorpoints <- function(data, labelsmap) {
  s1 <- sample(1:nrow(data), length(labelsmap), replace = TRUE)
  s2 <- sample(1:nrow(data), length(labelsmap), replace = TRUE)
  ap <- round((s1 + s2) / 2) # This biases the sample towards central anchor points
  ap <- as.list(ap)
  names(ap) <- names(labelsmap)
  return(ap)
}

labelsimulation <- function(data, labelsmap, xlim, ylim, ylim_n) {

  labellocations <- list()

  ap <- sampleanchorpoints(data, labelsmap)
  for (label in names(labelsmap)) {
    anchor <- c(stats::time(data)[ap[[label]]] + 1.0/(2*stats::frequency(data)), data[ap[[label]], label])
    labellocations[[label]] <- location.fromanchor(label, anchor, data, labelsmap, labellocations, xlim, ylim, ylim_n)
  }
  return(labellocations)
}
