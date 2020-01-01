#' @title Nonparametric kernel estimation
#' @description two methods of Nonparametric kernel estimation
#' @param h the bandwidth
#' @param y true distributions
#'  @param x random seq
#' @return NW kernel estimation \code{n}
#' @examples
#' \dontrun{
#' x <- seq(-1, 1, length = 40)
#' y <- 5 * x * cos(5 * pi * x)
#' h <- 0.055
#' NWsmooth.val <- NWSMOOTH(h, y, x)
#' }
#' @export
NWSMOOTH <- function(h, y, x) {
  n <- length(y) 
  s.hat <- rep(0, n) 
  for (i in 1:n) {
    a <- fx.hat(x[i], h) 
    s.hat[i] <- sum(y * a/sum(a))
  } 
  return(s.hat)
} 