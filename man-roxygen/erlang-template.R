#' @param shape shape parameter \eqn{n}, must be positive integer.
#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x"), "@param x quantile.", "") %>
#' <%=ifelse(exists("q", inherits = F), "@param q quantile.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("lower.tail"), "@param lower.tail logical; if \\code{TRUE} (default) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Erlang distribution with shape parameter \eqn{n} and rate parameter
#'   \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\beta^{n}}{\Gamma(n)} x^{n - 1}%
#'    \mathrm{e}^{-\beta x}}{f(x) = b^n / \Gamma(n) x^{n - 1} e^{-b x}}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta > 0}{b > 0}, \eqn{n \in \mathcal{N}^+}{n = 1, 2, 3, ...}
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{MGF_erlang}}}{ gives the moment generating function (MGF).}
#'  \item{\code{\link{derlang}}}{ gives the density function.}
#'  \item{\code{\link{perlang}}}{ gives the cumulative density function.}
#'  \item{\code{\link{E_erlang}}}{ gives the expected value.}
#'  \item{\code{\link{V_erlang}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_erlang}}}{ gives the kth moment.}
#'  \item{\code{\link{Etrunc_erlang}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_erlang}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_erlang}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_erlang}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_erlang}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_erlang}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Erlang Distribution
#'
