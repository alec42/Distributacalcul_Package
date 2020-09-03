#' Plot of simulated observations for copula
#'
#' @template copula-param-template
#' @template numberSimulations-template
#' @template seed-template
#' @template dependencyParameter-template
#'
#' @return Returns plot of simulated observations for the chosen copula.
#'
#' @importFrom rlang exec
#' @importFrom ggplot2 ggplot geom_hex aes scale_fill_gradient theme_classic labs
#'
#' @export
#'
simulatedCopulaHexPlot <- function(copula, numberSimulations = 1E4, seed = 42, dependencyParameter) {
    simulatedCopulaData <- rlang::exec(
        .fn = paste0("cr", copula),
        numberSimulations, seed,
        dependencyParameter
    )

    ggplot2::ggplot(data.frame(simulatedCopulaData), ggplot2::aes_string(x = 'X1', y = 'X2')) +
        ggplot2::geom_hex(bins = sqrt(numberSimulations)/2) +
        ggplot2::scale_fill_gradient(
            name = "Point\nintensity",
            low = "lightblue1", high = "darkblue"
        ) +
        ggplot2::theme_classic() +
        ggplot2::labs(
            title = "Simulated observations",
            subtitle = paste("with parameter value of", dependencyParameter),
            x = "U1",
            y = "U2"
        )
}
