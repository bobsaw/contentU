#' Calculate L1 Acceptance Value
#'
#' Calculate the L1 acceptance value from the assay values of a 10 dosage unit
#' sample.
#'
#' @param x A vector containing the individual assay values of 10 dosage units
#' @param T The target assay value.  T=100 in most circumstances.
#'
#' @return List containing the L1 Acceptance Value and a logical
#' indicating whether the sample passed.
#' @export
#'
#' @examples
#' sample <- seq(95, 104)
#' calcL1AV(sample)
calcL1AV <- function(x, T = 100) {
  if (length(x) != 10)
    warning("A sample of 10 dosage units is required.")

  xbar <- mean(x)
  s <- sd(x)
  k <- 2.4
  xbar <- mean(x)
  if (T <= 101.5) {
    if (xbar <= 101.5 && xbar >= 98.5) {
      M = xbar
    } else
      if (xbar < 98.5) {
        M = 98.5
      } else
        if (xbar > 101.5) {
          {
            M = 101.5
          }
        } else
          if (T > 101.5) {
            if (xbar >= 98.5 && xbar <= T) {
              M = xbar
            } else
              if (xbar < 98.5) {
                M = 98.5
              } else
                if (xbar > T) {
                  M = T
                }
          }
  }
  AV1 <- abs(M - xbar) + k * s
  if (AV1 <= 15.0) {
    passL1 <- TRUE
  } else
    passL1 <- FALSE

  return(list(AV1 = AV1, passL1 = passL1))

}


