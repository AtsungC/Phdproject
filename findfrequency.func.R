findfrequency <- function(x) {
  n <- length(x)
  x <- as.ts(x)
  # Remove trend from data
  x <- residuals(tslm(x ~ trend))
  # Compute spectrum by fitting ar model to largest section of x
  n.freq <- 500
  spec <- spec.ar(c(na.contiguous(x)), plot = FALSE, n.freq = n.freq)
  if (max(spec$spec) > 10) # Arbitrary threshold chosen by trial and error.
  {
    period <- floor(1 / spec$freq[which.max(spec$spec)] + 0.5)
    if (period == Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec) > 0)
      if (length(j) > 0) {
        nextmax <- j[1] + which.max(spec$spec[(j[1] + 1):n.freq])
        if (nextmax < length(spec$freq)) {
          period <- floor(1 / spec$freq[nextmax] + 0.5)
        } else {
          period <- 1L
        }
      }
      else {
        period <- 1L
      }
    }
  }
  else {
    period <- 1L
  }
  
  return(as.integer(period))
}

