testscaleoptions <- function(significand, minval, maxval, permittedsteps) {
  # Now we try the different combinations of and see which works best
  jointdeviation <- matrix(NA, length(PERMITTEDLABELS), length(permittedsteps))

  for (i in 1:length(PERMITTEDLABELS)) {
    step <- PERMITTEDLABELS[i]*10^significand
    minscale <- plyr::round_any(minval, step, f = floor)

    # This bit could be better. There's surely an algebraic way I can determine the best option here
    for (j in 1:length(permittedsteps)) {
      maxscale <- ((permittedsteps[j]-1)*step + minscale)
      if (maxscale > maxval) {
        jointdeviation[i,j] <- (minval - minscale) + (maxscale - maxval)
      } else {
        jointdeviation[i,j] <- NA
      }
    }
  }
  return(jointdeviation)
}

defaultscale <- function(data,permittedsteps=PERMITTEDSTEPS) {

  maxval <- max(data,na.rm=TRUE)
  minval <- min(data,na.rm=TRUE)
  span <- maxval-minval

  significand <- floor(log10(span/min(permittedsteps)))
  if (is.na(significand) || significand == Inf || significand == -Inf) {
    # Happens if span is zero, possibly other cases
    significand <- 0
  }

  jointdeviation <- testscaleoptions(significand, minval, maxval, permittedsteps)
  # Check here if none are feasible
  if (all(is.na(jointdeviation))) {
    # Try a larger significand
    significand <- significand + 1
    jointdeviation <- testscaleoptions(significand, minval, maxval, permittedsteps)
    if (all(is.na(jointdeviation))) {
      message("No feasible scale found, defaulting to something bad.")
      return(list("min" = minval, "max" = maxval, "nsteps" = 5))
    }
  }

  ideal <- which(jointdeviation == min(jointdeviation,na.rm=TRUE), arr.ind=TRUE)
  if (!is.null(nrow(ideal))) {
    ideal <- ideal[1, ] # Just take the first match
  }
  step <- PERMITTEDLABELS[ideal[1]]*10^significand
  nsteps <- permittedsteps[ideal[2]]
  minscale <- plyr::round_any(minval, step, f = floor)
  maxscale <- minscale + (nsteps-1)*step
  return(list("min" = minscale, "max" = maxscale, "nsteps" = nsteps))
}

createscale <- function(minscale,maxscale,nsteps,sigdig=2) {
  scale <- rep(NA,nsteps)
  stepsize <- (maxscale-minscale)/(nsteps-1)
  for (i in 1:nsteps) {
    scale[i] <- signif(minscale + (i-1)*stepsize,sigdig)
  }
  return(scale)
}

duplicateaxes <- function(toduplicate, panels, layout) {
  if (is.null(panels$panels[["1"]])) {
    toduplicate[["1"]] <- toduplicate[["2"]]
  }
  if (is.null(panels$panels[["2"]])) {
    toduplicate[["2"]] <- toduplicate[["1"]]
  }
  if (layout == "2b2" || layout == "2h") {
    if (is.null(panels$panels[["3"]])) {
      toduplicate[["3"]] <- toduplicate[["4"]]
    }
    if (is.null(panels$panels[["4"]])) {
      toduplicate[["4"]] <- toduplicate[["3"]]
    }
  }
  return(toduplicate)
}

handleunits <- function(panels, scaleunits, layout) {
  plist <- names(panels$panels)
  if (is.null(scaleunits)) {
    scaleunits <- DEFAULTSCALEUNITS
  }

  out <- list()
  if(!is.list(scaleunits)) {
    for (p in plist) {
      out[p] <- scaleunits
    }
  } else {
    for (p in plist) {
      if (p %in% names(scaleunits)) {
        out[p] <- scaleunits[[p]]
      } else {
        out[p] <- DEFAULTSCALEUNITS
      }
    }
  }
  out <- duplicateaxes(out, panels, layout)
  return(out)
}

ylimconform <- function(panels, ylim, data, layout) {
  ylim_list <- list()
  if (is.null(ylim)) {
    for (p in names(panels$panels)) {
      paneldf <- data[, panels$panels[[p]], drop = FALSE]
      if (any(!is.na(paneldf)) && is.finite(max(paneldf)) && is.finite(min(paneldf))) {
        ylim_list[[p]] <- defaultscale(paneldf)
      } else {
        ylim_list[[p]] <- EMPTYSCALE
      }
    }
  } else {
    if ("min" %in% names(ylim) || "max" %in% names(ylim) || "nsteps" %in% names(ylim)) {
      # have supplied a single list to apply to all
      if (is.null(ylim$nsteps) || ylim$nsteps < 2) {
        stop("You must supply nsteps > 2 for the y limit.")
      }
      if (is.null(ylim$max)) {
        stop("You did not supply a max ylimit.")
      }
      for (p in names(panels$panels)) {
        ylim_list[[p]] <- ylim
      }
    } else {
      # have supplied lims for each
      for (p in names(panels$panels)) {
        if (p %in% names(ylim)) {
          ylim_list[[p]] <- ylim[[p]]
          if (is.null(ylim[[p]]) || ylim[[p]]$nsteps < 2) {
            stop(paste("The y-limit you supplied for panel ", p, " has fewer than 2 points (or you forgot to supply nsteps).", sep = ""))
          }
          if (is.null(ylim[[p]]$max)) {
            stop(paste("You did not supply a max ylimit for panel ", p, ".", step = ""))
          }
          if (is.null(ylim[[p]]$min)) {
            stop(paste("You did not supply a max ylimit for panel ", p, ".", step = ""))
          }
        } else {
          paneldf <- data[, panels$panels[[p]], drop = FALSE]
          if (ncol(paneldf) > 0) {
            ylim_list[[p]] <- defaultscale(paneldf)
          } else {
            ylim_list[[p]] <- EMPTYSCALE
          }
        }
      }
    }
  }
  ylim_list <- duplicateaxes(ylim_list, panels, layout)
  return(ylim_list)
}

handleticks <- function(data, panels, ylim) {
  ticks <- list()
  for (p in names(panels$panels)) {
    if (!is.null(ylim[[p]])) {
      ticks[[p]] <- createscale(ylim[[p]]$min, ylim[[p]]$max, ylim[[p]]$nsteps)
    }
  }
  return(ticks)
}

xlabels <- function(xlim) {
  # Ideally, every year, but no more than max-x-years (set in constants) labels because it gets too crowded
  startyear <- xlim[1]
  endyear <- xlim[2]
  range <- endyear - startyear + 1

  step <- ceiling(range/MAXXYEARS)

  # Create the sequence and offset the labels by half a year so that the labels are centered
  labels <- seq(from = startyear, to = endyear, by = step)
  # Now shift the at
  at <- labels + 0.5
  return(list("at" = at, "labels" = labels))
}

handlexlabels <- function(panels, xlim) {
  out <- list()
  for (p in names(panels$panels)) {
    out[[p]] <- xlabels(xlim[[p]])
  }
  return(out)
}

warnifxdiff <- function(panels, xlim) {
  for (p in names(xlim)) {
    for (q in names(xlim)) {
      if (all(xlim[[p]] != xlim[[q]])) {
        warning(paste("Panels ", p, " and ", q, " have differing x limits. This may lead to confusing graphs. Be careful!", sep = ""))
      }
    }
  }
}

xlimconform <- function(panels, xlim, data) {
  # Only allows whole years at this stage
  default <- c(floor(min(stats::time(data))), ceiling(max(stats::time(data))))
  out <- list()
  if (!is.list(xlim)) {
    for (p in names(panels$panels)) {
      if (is.null(xlim)) {
        out[[p]] <- default
      } else {
        out[[p]] <- xlim
      }
    }
  } else {
    for (p in names(panels$panels)) {
      if (p %in% names(xlim)) {
        out[[p]] <- xlim[[p]]
      } else {
        out[[p]] <- default
      }
    }
    # have a check for non-matching xlimits
    warnifxdiff(panels, out)
  }
  return(out)
}
