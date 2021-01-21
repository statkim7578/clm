GEST <- function(th, dat, tloc, cond) {
  nr <- nrow(dat)
  sty <- dat$sty

  if (tloc == 1) {
    num1 <- exp(th[10] * dat$x1)
    num2 <- exp(th[1] + th[10] * dat$x2)
    num3 <- exp(th[2] + th[10] * dat$x3)
    num4 <- exp(th[3] + th[10] * dat$x4)
    num5 <- exp(th[4] + th[10] * dat$x5)
    num6 <- exp(th[5] + th[10] * dat$x6)
    num7 <- exp(th[6] + th[10] * dat$x7)
    num8 <- exp(th[7] + th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }


  if (tloc == 2) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[10] * dat$x2)
    num3 <- exp(th[2] + th[10] * dat$x3)
    num4 <- exp(th[3] + th[10] * dat$x4)
    num5 <- exp(th[4] + th[10] * dat$x5)
    num6 <- exp(th[5] + th[10] * dat$x6)
    num7 <- exp(th[6] + th[10] * dat$x7)
    num8 <- exp(th[7] + th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }


  if (tloc == 3) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[3] * dat$x2)
    num3 <- exp(th[10] * dat$x3)
    num4 <- exp(th[3] + th[10] * dat$x4)
    num5 <- exp(th[4] + th[10] * dat$x5)
    num6 <- exp(th[5] + th[10] * dat$x6)
    num7 <- exp(th[6] + th[10] * dat$x7)
    num8 <- exp(th[7] + th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }

  if (tloc == 4) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[10] * dat$x2)
    num3 <- exp(th[3] + th[10] * dat$x3)
    num4 <- exp(th[10] * dat$x4)
    num5 <- exp(th[4] + th[10] * dat$x5)
    num6 <- exp(th[5] + th[10] * dat$x6)
    num7 <- exp(th[6] + th[10] * dat$x7)
    num8 <- exp(th[7] + th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }

  if (tloc == 5) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[10] * dat$x2)
    num3 <- exp(th[3] + th[10] * dat$x3)
    num4 <- exp(th[4] + th[10] * dat$x4)
    num5 <- exp(th[10] * dat$x5)
    num6 <- exp(th[5] + th[10] * dat$x6)
    num7 <- exp(th[6] + th[10] * dat$x7)
    num8 <- exp(th[7] + th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }

  if (tloc == 6) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[10] * dat$x2)
    num3 <- exp(th[3] + th[10] * dat$x3)
    num4 <- exp(th[4] + th[10] * dat$x4)
    num5 <- exp(th[5] + th[10] * dat$x5)
    num6 <- exp(th[10] * dat$x6)
    num7 <- exp(th[6] + th[10] * dat$x7)
    num8 <- exp(th[7] + th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }

  if (tloc == 7) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[10] * dat$x2)
    num3 <- exp(th[3] + th[10] * dat$x3)
    num4 <- exp(th[4] + th[10] * dat$x4)
    num5 <- exp(th[5] + th[10] * dat$x5)
    num6 <- exp(th[6] + th[10] * dat$x6)
    num7 <- exp(th[10] * dat$x7)
    num8 <- exp(th[7] + th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }

  if (tloc == 8) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[10] * dat$x2)
    num3 <- exp(th[3] + th[10] * dat$x3)
    num4 <- exp(th[4] + th[10] * dat$x4)
    num5 <- exp(th[5] + th[10] * dat$x5)
    num6 <- exp(th[6] + th[10] * dat$x6)
    num7 <- exp(th[7] + th[10] * dat$x7)
    num8 <- exp(th[10] * dat$x8)
    num9 <- exp(th[8] + th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }

  if (tloc == 9) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[10] * dat$x2)
    num3 <- exp(th[3] + th[10] * dat$x3)
    num4 <- exp(th[4] + th[10] * dat$x4)
    num5 <- exp(th[5] + th[10] * dat$x5)
    num6 <- exp(th[6] + th[10] * dat$x6)
    num7 <- exp(th[7] + th[10] * dat$x7)
    num8 <- exp(th[8] + th[10] * dat$x8)
    num9 <- exp(th[10] * dat$x9)
    num10 <- exp(th[9] + th[10] * dat$x10)
  }

  if (tloc == 10) {
    num1 <- exp(th[1] + th[10] * dat$x1)
    num2 <- exp(th[2] + th[10] * dat$x2)
    num3 <- exp(th[3] + th[10] * dat$x3)
    num4 <- exp(th[4] + th[10] * dat$x4)
    num5 <- exp(th[5] + th[10] * dat$x5)
    num6 <- exp(th[6] + th[10] * dat$x6)
    num7 <- exp(th[7] + th[10] * dat$x7)
    num8 <- exp(th[8] + th[10] * dat$x8)
    num9 <- exp(th[9] + th[10] * dat$x9)
    num10 <- exp(th[10] * dat$x10)
  }

  dnum1 <- num4 + num9 + num10
  dnum2 <- num4 + num7 + num8
  dnum3 <- num4 + num7 + num8 + num9 + num10
  dnum4 <- num4 + num6 + num7 + num8 + num9 + num10
  dnum5 <- num3 + num4 + num10
  dnum6 <- num3 + num4 + num7 + num8 + num9 + num10
  dnum7 <- num3 + num4 + num6 + num7 + num8 + num10
  dnum8 <- num3 + num4 + num6 + num7 + num8 + num9 + num10
  dnum9 <- num2 + num3 + num4 + num6 + num7 + num8 + num9 + num10
  dnum10 <- num1 + num2 + num3 + num4 + num6 + num7 + num8 + num9 + num10
  dnum11 <- num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8 + num9 +
    num10

  q <- rep(0, nr)
  q[which(sty == 1)] <- dnum1[which(sty == 1)]
  q[which(sty == 2)] <- dnum2[which(sty == 2)]
  q[which(sty == 3)] <- dnum3[which(sty == 3)]
  q[which(sty == 4)] <- dnum4[which(sty == 4)]
  q[which(sty == 5)] <- dnum5[which(sty == 5)]
  q[which(sty == 6)] <- dnum6[which(sty == 6)]
  q[which(sty == 7)] <- dnum7[which(sty == 7)]
  q[which(sty == 8)] <- dnum8[which(sty == 8)]
  q[which(sty == 9)] <- dnum9[which(sty == 9)]
  q[which(sty == 10)] <- dnum10[which(sty == 10)]
  q[which(sty == 11)] <- dnum11[which(sty == 11)]

  rst <- log(nr * cond) - log(sum(1/q))
  return(rst)
}
