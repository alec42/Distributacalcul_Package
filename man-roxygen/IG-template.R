#' @param mean mean (location) parameter \eqn{\mu}{mu}, must be positive.
#' @param shape shape parameter \eqn{\beta}{beta}, must be positive.
#' @param dispersion alternative parameterization to the shape parameter, dispersion = 1 / rate.
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
#'  \item{\code{\link{MGF_IG}}}{ gives the moment generating function (MGF).}
#'  \item{\code{\link{E_IG}}}{ gives the expected value.}
#'  \item{\code{\link{V_IG}}}{ gives the variance.}
#'  \item{\code{\link{Etrunc_IG}}}{ gives the truncated mean.}
#'  \item{\code{\link{SL_IG}}}{ gives the stop-loss.}
#'  \item{\code{\link{Elim_IG}}}{ gives the limited mean.}
#'  \item{\code{\link{Mexcess_IG}}}{ gives the mean excess loss.}
#'  \item{\code{\link{TVaR_IG}}}{ gives the Tail Value-at-Risk.}
#'  \item{\code{\link{VaR_IG}}}{ gives the Value-at-Risk.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Inverse Gaussian Distribution
#'
