#' Calculates difference between eCDFs
#'
#' Returns a data frame of differences in the empirical distribution functions.
#'
#' Given vectors \eqn{x_1 \sim F_1} and \eqn{x_2 \sim F_2}, this function
#' calculates \eqn{F_1(x) - F_2(x)} for \eqn{x} defined by \code{breaks}.
#' Confidence intervals are derived using \link[DescTools]{BinomDiffCI} with
#' the Mee (1984) and Farrington-Manning (1990) method for difference in
#' binomial proportions confidence interval.
#'
#' @param x1,x2 Numeric vector of observations
#' @param breaks Values to calculate eCDF difference
#' @param conf_level Confidence level
#'
#' @return A data frame of the value, estimated difference and confidence
#' limits.
#'
#' @examples
#' x1 <- sample(-100:100, 25)
#' x2 <- sample(-100:100, 20)
#' ecdf_diff(x1, x2)
#'
#' @importFrom stats ecdf
#'
#' @export
ecdf_diff <- function(x1, x2, breaks = seq(-100, 100, 1),
                      conf_level = 0.95) {
  ecdf1 <- ecdf(x1)
  ecdf2 <- ecdf(x2)
  n1 <- get("nobs", envir = environment(ecdf1))
  n2 <- get("nobs", envir = environment(ecdf2))
  out <- t(sapply(breaks, function(y)
    DescTools::BinomDiffCI(ecdf1(y)*n1, n1,
                           ecdf2(y)*n2, n2, conf.level = conf_level,
                           method = "mee")))
  colnames(out) <- c("Est", "LCL", "UCL")
  data.frame(Value = breaks, as.data.frame(out))
}
