#' @param min,max lower and upper limits of the distribution. Must be finite.
#'
#' <%=ifelse(exists("x"), "@param x quantile. By definition, it has no impact on the uniform distribution. Set to 1 by default.", "") %>
#' <%=ifelse(exists("q", inherits = F), "@param q quantile.", "") %>
#'
#' @details
#'  The (discrete) uniform distribution with min and max parameters \eqn{a}
#'   and \eqn{b} respectively has density:
#'   \deqn{\textrm{Pr}\left(X = x \right) = \frac{1}{b - a + 1}}{Pr(X = x) = 1 / (b - a + 1)}
#'   for \eqn{x \in \{a, a + 1, \dots, b - 1, b\}}{x = a, a + 1, ..., b - 1, b}.
#'
#' @return
#'  Function :
#'  \itemize{
#'  \item{\code{\link{E_unifD}}}{ gives the expected value.}
#'  \item{\code{\link{V_unifD}}}{ gives the variance.}
#'  \item{\code{\link{d_unifD}}}{ gives the density function.}
#'  \item{\code{\link{d_unifD}}}{ gives the cumulative density function.}
#'  }
#'  Invalid parameter values will return an error detailing which parameter is problematic.
#'
#' @family Discrete Uniform Distribution
#'
