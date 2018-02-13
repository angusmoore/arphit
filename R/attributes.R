fillwith <- function(tofill, default) {
  out <- list()
  ct <- 1
  for (s in tofill) {
    # Give it a default
    out[s] <- default[ct]
    ct <- ct + 1
    # Reset the default cycle counter if necessary
    if (ct > length(default)) {
      ct <- ct %% length(default)
      if (ct == 0) {
        ct <- 1 # handles defaults with length 1, because mod 1 returns 0
      }
    }
  }
  return(out)
}

handleattribute <- function(series, att, default) {
  if (is.null(att)) {
    return(fillwith(series, default))
  } else if (!is.list(att)) {
    return(fillwith(series, att))
  } else {
    # Have supplied a list
    out <- list()
    ct <- 1
    for (s in series) {
      if (s %in% names(att)) {
        # Has been supplied
        out[s] <- att[s]
      } else {
        # Give it a default
        out[s] <- default[ct]
        ct <- ct + 1
        # Reset the default cycle counter if necessary
        if (ct > length(default)) {
          ct <- ct %% length(default)
          if (ct == 0) {
            ct <- 1 # handles defaults with length 1, because mod 1 returns 0
          }
        }
      }
    }
    return(out)
  }
}

convert2perpanel <- function(values) {
  if (!is.null(values) && !("1" %in% names(values) || "2"  %in% names(values) || "3" %in% names(values) || "4" %in% names(values))) {
    # Must be a non-nested list
	out <- list()
    for (p in c("1", "2", "3", "4")) {
      out[[p]] <- values
    }
    return(out)
  } else {
	  return(values)
  }
}

handleattributes <- function(panels, colin, pchin, ltyin, lwdin, barcolin) {
  colin <- convert2perpanel(colin)
  pchin <- convert2perpanel(pchin)
  ltyin <- convert2perpanel(ltyin)
  lwdin <- convert2perpanel(lwdin)
  barcolin <- convert2perpanel(barcolin)

  attributes <- list()
  for (p in names(panels)) {
    col <- handleattribute(panels[[p]], colin[[p]], DEFAULTCOLORS)
  	pch <- handleattribute(panels[[p]], pchin[[p]], DEFAULTPCH)
  	lty <- handleattribute(panels[[p]], ltyin[[p]], DEFAULTLTY)
  	lwd <- handleattribute(panels[[p]], lwdin[[p]], DEFAULTLWD)
  	barcol <- handleattribute(panels[[p]], barcolin, DEFAULTBARCOL)
  	attributes[[p]] <- list(col = col, pch = pch, lty = lty, lwd = lwd, barcol = barcol)
  }
  return(attributes)
}
