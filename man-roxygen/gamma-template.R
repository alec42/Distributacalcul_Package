#' @param shape shape parameter \eqn{\alpha}{alpha}, must be positive integer.
#' @param rate \eqn{\beta}{beta} is the rate parameter, must be positive.
#' @param scale alternative parameterization to rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Gamma distribution with shape parameter \eqn{\alpha}{a} and rate
#'    parameter \eqn{\beta}{b} has density:
#'   \deqn{f\left(x\right) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1}%
#'    \textrm{e}^{-\beta x}}{f(x) = b^a / \Gamma(a) x^{a - 1} e^{-b x}}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\beta, \alpha > 0}{b, a > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{MGF_gamma}}}{ gives the moment generating function (MGF).}
#'  \item{\code{\link{E_gamma}}}{ gives the expected value.}
#'  \item{\code{\link{V_gamma}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_gamma}}}{ gives the kth moment.}
#'  \item{\code{\link{Etrunc_gamma}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_gamma}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_gamma}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_gamma}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_gamma}}}{ gives the Tail Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Gamma Distribution
#'
