findcandidates <- function(data, labelsmap, xlim, ylim, ylim_n) {
  candidates <- list()
  for (i in 1:AUTOLABELATTEMPTS) {
    message(paste("Finding candidate for label locations (attempt ", i, " of ", AUTOLABELATTEMPTS, ")", sep = ""))
    candidates[[i]] <- labelsimulation(data, labelsmap, xlim, ylim, ylim_n)
  }
  return(candidates)
}

autolabel <- function(data, panels, shading, layout, xlim_list, ylim_list, col, bgshading, lines, arrows, labels) {
  newlabels <- list()
  newarrows <- list()
  for (p in c("1","2","3","4")) {
    if (notalreadylabelled(p, labels) && notRHS(p, layout)) {
      labelsmap <- createlabels(data, panels, p, layout)
      if (length(labelsmap) > 1) {
        data <- convertdata.axes(data, panels, p, layout, ylim_list)

        xlim <- xlim_list[[p]]
        ylim <- c(ylim_list[[p]]$min, ylim_list[[p]]$max)
        ylim_n <- ylim_list[[p]]$nsteps

        # Blank plot ensures the par("pin") calls return the right dimensions
        l <- getlocation(p ,layout)
        graphics::par(mfg = l)
        graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "", xlim = xlim, ylim = ylim)

        ## TODO: Add collisions for bg shading, lines and arrows
        candidates <- findcandidates(data, labelsmap, xlim, ylim, ylim_n)
        locations <- bestcandidate(candidates, data, labelsmap)
        newlabels <- append(newlabels, formatlabels(locations, labelsmap, col, p))
        newarrows <- append(newarrows, addarrows(data, panels, labelsmap, locations, col, p))
        if (length(locations) < length(labelsmap)) {
          warning("Unable to find locations for some series labels.")
        }
      }
    }
  }
  return(list(labels = newlabels, arrows = newarrows))
}
