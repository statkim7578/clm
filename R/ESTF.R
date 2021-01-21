ESTF <- function(odat) {
  est <- ESTF5(odat)
  parest <- est[[1]][1:9]
  bp <- which(parest == min(parest))  ### bp: baseline prod
  test <- sum(parest < 0)

  if (test > 0) {
    if (bp == 1)
      est <- ESTF1(odat)
    if (bp == 2)
      est <- ESTF2(odat)
    if (bp == 3)
      est <- ESTF3(odat)
    if (bp == 4)
      est <- ESTF4(odat)
    if (bp == 5)
      est <- ESTF6(odat)
    if (bp == 6)
      est <- ESTF7(odat)
    if (bp == 7)
      est <- ESTF8(odat)
    if (bp == 8)
      est <- ESTF9(odat)
    if (bp == 9)
      est <- ESTF10(odat)
  }

  th <- est[[1]]
  varth <- -solve(est[[2]])
  return(list(th = th, varth = varth, tloc = bp))
}


