#USP <905> Uniforimty of Dosage Units


# Calculate L1 Acceptance Value -------------------------------------------

calcL1AV <- function(x, T=100){
  if(length(x)!=10) warning("A sample of 10 dosage units is required.")

  xbar <- mean(x)
  s <- sd(x)
  k <- 2.4
  xbar <- mean(x)
  if(T<=101.5) {
    if(xbar <= 101.5 && xbar >= 98.5) {M=xbar} else
      if(xbar<98.5) {M=98.5} else
        if(xbar>101.5) {{M=101.5}
     } else
    if(T>101.5) {
      if(xbar>=98.5 && xbar<= T){M=xbar} else
        if(xbar<98.5){M=98.5} else
          if(xbar>T){M=T}
    }
 }
  AV1 <- abs(M-xbar) + k*s
  if(AV1<=15.0) {passL1 <- TRUE} else
    passL1 <- FALSE

  return(list(AV1=AV1, passL1=passL1))

}


