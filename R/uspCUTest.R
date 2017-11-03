
# Conduct the USP <905> Content Uniformity Test------------------

uspCUTest <- function(sample10, sample20=NULL, T=100){
  cul1 <- calcL1AV(sample10, T=T)
  if(cul1[[1]]<=15.0) {return(c(cul1, AV2=NA, passL2=NA))} else
    if(length(sample20 != 20)) {
      cul2 <- calcL2AV(sample10, sample20, T=T)
      return(c(cul1, cul2))
      } else
        print("L1 failure.  An additional 20 samples will be required for L2.")
}