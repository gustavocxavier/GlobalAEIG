winsorizar <- function (x, fraction=0.01) {
  # Source: https://www.r-bloggers.com/winsorization/
  #
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) { stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

NW <- function (x, Lag=NULL) {
  require(lmtest)
  require(sandwich)
  coeftest(x, vcov=NeweyWest(x, lag = Lag, prewhite = FALSE))
}

