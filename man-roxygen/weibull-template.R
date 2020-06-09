#' @param shape shape parameter \eqn{\tau}{tau}, must be positive integer.
#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Weibull distribution with shape parameter \eqn{\tau}{t} and rate parameter
#'   \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \beta \tau \left( \beta x \right) ^{\tau -1} %
#'    \mathrm{e}^{-\left( \beta x\right) ^{\tau }}}{f(x) = b t (b x)^(t - 1) %
#'    e^{(-b x)^t}}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}, \eqn{\tau > 0}{t > 0}
#'
#' @return Returns numeric value.
#'  Function :
#'  \itemize{
#'  \item{\code{\link{E_weibull}}}{ gives the expected value.}
#'  \item{\code{\link{V_weibull}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_weibull}}}{ gives the kth moment.}
#'  \item{\code{\link{Etrunc_weibull}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_weibull}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_weibull}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_weibull}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_weibull}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_weibull}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Weibull Distribution
#'
