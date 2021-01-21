##################################################################################
DataGen <- function(n, type, beta, alpha, sind) {
  id <- 1:n

  if (sind == 1)
    sty <- rep(11, n)
  if (sind == 2)
    sty <- sample(1:11, n, replace = T, prob = c(0.05, 0.05, 0.05,
                                                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.5))
  if (sind == 3)
    sty <- sample(1:11, n, replace = T)

  x1 <- rgamma(n, 4, 3)
  x2 <- rgamma(n, 5, 3)
  x3 <- rgamma(n, 6, 2)
  x4 <- rgamma(n, 4, 3)
  x5 <- rgamma(n, 5, 3)
  x6 <- rgamma(n, 6, 2)
  x7 <- rgamma(n, 4, 3)
  x8 <- rgamma(n, 5, 3)
  x9 <- rgamma(n, 6, 2)
  x10 <- rgamma(n, 4, 2)
  x <- cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

  x[sty == 1, c(1:3, 5:8)] <- 0
  x[sty == 2, c(1:3, 5:6, 9:10)] <- 0
  x[sty == 3, c(1:3, 5:6)] <- 0
  x[sty == 4, c(1:3, 5)] <- 0
  x[sty == 5, c(1:2, 5:9)] <- 0
  x[sty == 6, c(1:2, 5:6)] <- 0
  x[sty == 7, c(1:2, 5, 9)] <- 0
  x[sty == 8, c(1:2, 5)] <- 0
  x[sty == 9, c(1, 5)] <- 0
  x[sty == 10, 5] <- 0

  cutil <- Utility(sty, type, beta, alpha, x)
  cprob <- lapply(cutil, function(s) s/sum(s))
  deci0 <- lapply(cprob, rmultinom, n = 1, size = 1)
  decision <- sapply(1:n, function(s) {
    stype <- sty[s]
    choice <- c(type[[stype]], 11)
    cloc <- which(deci0[[s]] == 1)
    choice[cloc]
  })


  rst1 <- data.frame(id, sty, decision, x)
  oloc <- which(rst1$decision != 11)
  rst2 <- rst1[oloc, ]

  return(list(rst1, rst2))
}
