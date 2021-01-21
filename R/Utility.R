Utility <- function(stype, type, beta, alpha, x) {
  n <- length(stype)
  rst <- list()
  for (i in 1:n) {
    sn <- stype[i]
    sty <- type[[sn]]
    betaz <- beta[sty]
    z <- x[i, sty]
    rst[[i]] <- c(exp(betaz + alpha * z), 1)
  }
  return(rst)
}
