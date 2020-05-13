#' @param mean mean \eqn{\mu}{mu}.
#' @param sd standard deviation \eqn{\sigma}{sigma}
#'
#' <%=ifelse(exists("d"), "@param d cut-off value.", "") %>
#' <%=ifelse(exists("kap"), "@param kap probability.", "") %>
#' <%=ifelse(exists("k"), "@param k kth-moment.", "") %>
#' <%=ifelse(exists("less.than.d"), "@param less.than.d logical; if \\code{TRUE} (défaut) truncated mean for values <= d, otherwise, for values > d.", "") %>
#'
#' @details
#'  The Normal distribution with mean \eqn{\mu}{mu} and standard deviation
#'   \eqn{\sigma}{sigma} has density:
#'   \deqn{\frac{1}{\sqrt{2\pi}\sigma}\textrm{e}^{-\frac{1}{2}\left(\frac{x - \mu}{\sigma}\right)^2}}{f(x) = e^(-(1/2) ((x - mu)/sigma)^2) / ((2 pi)^(1/2) sigma}
#'   for \eqn{x \in \mathcal{R}}{x real}, \eqn{\mu \in \mathcal{R}, \sigma > 0}{mu real, sigma > 0}.
#'
#' @family Normal distribution
#'
