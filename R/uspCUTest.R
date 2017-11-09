#' USP Content Uniformity test
#'
#' @param sample10 A vector containing the individual assay values of 10 dosage
#' units.
#' @param sample20 A vector containing the individual assay values of an
#' additional 20 dosage units.
#' @param T The target assay value.  T=100 in most circumstances.
#'
#' @return List the L1 Acceptance Value, a logical indicating
#' whether the sample passed L1, the L2 Acceptance Value and a
#' logical indicating whether the sample passed
#' @export
#' sample1 <- seq(95, 104)
#' sample2 <- seq(95, 104.5, by=0.5)
#' uspCUTest(sample1, sample2)
#' @examples
uspCUTest <- function(sample10,
                      sample20 = NULL,
                      T = 100) {
  cul1 <- calcL1AV(sample10, T = T)
  if (cul1[[1]] <= 15.0) {
    return(c(cul1, AV2 = NA, passL2 = NA))
  } else
    if (length(sample20 != 20)) {
      cul2 <- calcL2AV(sample10, sample20, T = T)
      return(c(cul1, cul2))
    } else
      print("L1 failure.  An additional 20 samples will be required for L2.")
}
