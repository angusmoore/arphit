drawbgshading <- function(shading) {
  # if any are NA, give them the ends of the plot
  if (is.na(shading$x1)) {
    shading$x1 <- graphics::par("usr")[1]
  }
  if (is.na(shading$x2)) {
    shading$x2 <- graphics::par("usr")[2]
  }
  if (is.na(shading$y1)) {
    shading$y1 <- graphics::par("usr")[3]
  }
  if (is.na(shading$y2)) {
    shading$y2 <- graphics::par("usr")[4]
  }
  graphics::rect(shading$x1, shading$y1, shading$x2, shading$y2, col = shading$colour, lty = 0)
}

drawbgshadings <- function(shadings, p) {
  for (shading in shadings) {
    if (shading$panel == p) {
      drawbgshading(shading)
    }
  }
}

drawannotationline <- function(line) {
  # if any are NA, give them the ends of the plot
  if (is.na(line$x1)) {
    line$x1 <- graphics::par("usr")[1]
  }
  if (is.na(line$x2)) {
    line$x2 <- graphics::par("usr")[2]
  }
  if (is.na(line$y1)) {
    line$y1 <- graphics::par("usr")[3]
  }
  if (is.na(line$y2)) {
    line$y2 <- graphics::par("usr")[4]
  }
  graphics::segments(line$x1, line$y1, line$x2, line$y2, col = line$colour, lty = line$lty, lwd = line$lwd)
}

drawannotationlines <- function(lines, p) {
  for (line in lines) {
    if (line$panel == p) {
      drawannotationline(line)
    }
  }
}

drawlabel <- function(label) {
  graphics::text(label$text, x = label$x, y = label$y, adj = c(0.5, 0.5), col = label$colour, cex = label$cex)
}

drawlabels <- function(labels, p) {
  for (label in labels) {
    if (label$panel == p) {
      drawlabel(label)
    }
  }
}

drawarrow <- function(arrow) {
  graphics::arrows(arrow$tail.x, arrow$tail.y, arrow$head.x, arrow$head.y, col = arrow$colour, lwd = arrow$lwd, length = 0.1)
}

drawarrows <- function(arrows, p) {
  for (arrow in arrows) {
    if (arrow$panel == p) {
      drawarrow(arrow)
    }
  }
}
