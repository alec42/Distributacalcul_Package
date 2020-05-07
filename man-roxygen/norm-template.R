#' @param mean mean \eqn{\mu}{mu}.
#' @param sd standard deviation \eqn{\sigma}{sigma}
#'
#' <%=ifelse(exists("k") && k, "@param k kth-moment.", "") %>
#' <%=ifelse(exists("x") && x, "@param x quantile.", "") %>
#' <%=ifelse(exists("q") && q, "@param q quantile.", "") %>
#' <%=ifelse(exists("d") && d, "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap") && kap, "@param kap probability.", "") %>
#' <%=ifelse(exists("lower.tail") && lower.tail, "@param lower.tail logical; if \\code{TRUE} (défaut) probabilities are Pr(M <= k), otherwise, Pr(M > k).", "") %>
#'
#' @details
#'  The Normal distribution with mean \eqn{\mu}{mu} and standard deviation
#'   \eqn{\sigma}{sigma} has density:
#'   \deqn{\frac{1}{\sqrt{2\pi}\sigma}\textrm{e}^{-\frac{1}{2}\left(\frac{x - \mu}{\sigma}\right)^2}}{f(x) = e^(-(1/2) ((x - mu)/sigma)^2) / ((2 pi)^(1/2) sigma}
#'   for \eqn{x \in \mathcal{R}}{x real}, \eqn{\mu \in \mathcal{R}, \sigma > 0}{mu real, sigma > 0}.
#'
#' @family Normal distribution
#'
