sanitycheckdata <- function(series, data) {
  for (p in series) {
    for (s in p) {
      if (!s %in% colnames(data)) {
        stop(paste("Series ", s, " is not in your dataset.", sep = ""))
      }
    }
  }
}
