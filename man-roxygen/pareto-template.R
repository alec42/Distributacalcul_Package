#' @param shape shape parameter \eqn{\alpha}{alpha}, must be positive.
#' @param rate \eqn{\lambda}{lambda} rate parameter, must be positive.
#' @param scale alternative parameterization to the rate parameter, scale = 1 / rate.
#'
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (default) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Pareto distribution with rate parameter \eqn{\lambda}{lam} as well as shape
#'   parameter \eqn{\alpha}{a} has density: \deqn{f\left(x\right) = \frac{\alpha%
#'   \lambda^{\alpha}}{(\lambda + x)^{\alpha + 1}}}{f(x) = (a lam^a)/ (lam + x)^(a + 1)}
#'   for \eqn{x \in \mathcal{R}^+}{x > 0}, \eqn{\alpha, \lambda > 0}{a, lam > 0}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{d_pareto}}}{ gives the density function.}
#'  \item{\code{\link{p_pareto}}}{ gives the cumulative density function.}
#'  \item{\code{\link{E_pareto}}}{ gives the expected value.}
#'  \item{\code{\link{V_pareto}}}{ gives the variance.}
#'  \item{\code{\link{kthmoment_pareto}}}{ gives the kth moment.}
#'  \item{\code{\link{Etronq_pareto}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_pareto}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_pareto}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_pareto}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_pareto}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_pareto}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Pareto Distribution
#'
