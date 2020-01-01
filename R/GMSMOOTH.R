#' @title Nonparametric kernel estimation
#' @description two methods of Nonparametric kernel estimation
#' @param y true distributions
#'  @param x random seq
#' @param h the bandwidth
#' @return GM kernel estimation \code{n}
#' @examples
#' \dontrun{
#' x <- seq(-1, 1, length = 40)
#' y <- 5 * x * cos(5 * pi * x)
#' h <- 0.055
#' GMsmooth.val <- GMSMOOTH(y, x, h)
#' }
#' @export
GMSMOOTH <- function(y, x, h) {
  n <- length(y)
  s <- c(-Inf, 0.5 * (x[-n] + x[-1]), Inf) 
  s.hat <- rep(0, n) 
  for (i in 1:n) {
    fx.hat <- function(z, h, x) { 
      dnorm((x - z)/h)/h
    } 
    a <- y[i] * integrate(fx.hat, s[i], s[i + 1], h =
                            h, x = x[i])$value 
    s.hat[i] <- sum(a)
  }
  return(s.hat)
} 