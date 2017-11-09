#' Calculate L2 Acceptance Value
#'
#' Calculate the L2 acceptance value from the assay values of a 30-unit sample
#' of dosage units.  Usually applied after an L1 failure.
#'
#' @param sample10 A vector containing the individual assay values of 10 dosage
#' units.
#' @param sample20 A vector containing the individual assay values of an
#' additional 20 dosage units.
#' @param T The target assay value.  T=100 in most circumstances.
#'
#' @return List the L2 Acceptance Value and a logical
#' indicating whether the sample passed.
#' @export
#'
#' @examples
#' sample1 <- seq(95, 104)
#' sample2 <- seq(95, 104.5, by=0.5)
#' calcL2AV(sample1, sample2)
calcL2AV <- function(sample10, sample20, T = 100) {
  x <- c(sample10, sample20)
  if (length(x) != 30)
    warning("A sample of 30 dosage units is required for L2 AV.")

  L2 <- 25.0
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
  AV2 <- abs(M - xbar) + k * s
  indivUnitTest <- (x < (100 - 0.01 * L2) | x > (100 + 0.01 * L2))
  if (AV2 <= 15.0 && sum(indivUnitTest == 0)) {
    passL2 <- TRUE
  } else
    passL2 <- FALSE

  return(list(AV2 = AV2, passL2 = passL2))
}
