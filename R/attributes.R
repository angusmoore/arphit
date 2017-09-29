fillwith <- function(tofill, default) {
  out <- list()
  ct <- 1
  for (s in names(tofill)) {
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
    for (s in names(series)) {
      if (s %in% names(att)) {
        # Has been suplied
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

handleattributes <- function(series, colin, pchin, ltyin, lwdin, barcolin) {
  col <- handleattribute(series, colin, DEFAULTCOLORS)
  pch <- handleattribute(series, pchin, DEFAULTPCH)
  lty <- handleattribute(series, ltyin, DEFAULTLTY)
  lwd <- handleattribute(series, lwdin, DEFAULTLWD)
  barcol <- handleattribute(series, barcolin, DEFAULTBARCOL)
  return(list("col" = col, "pch" = pch, "lty" = lty, "lwd" = lwd, "barcol" = barcol))
}
