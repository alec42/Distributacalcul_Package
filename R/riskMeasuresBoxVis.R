#' Interactive risk measures visualization (server side)
#'
#' @param input input for server side.
#' @param output output for server side.
#' @param session session for server side.
#' @param law Distribution to visualize, one of ...
#'
#' @return Server function for the risk measures module.
#'  Should not be run directly.
#'
#' @importFrom rlang exec ns_env
#' @importFrom dplyr case_when
#' @importFrom ggplot2 ggplot stat_function aes labs theme_classic
#' @importFrom plotly renderPlotly
#' @importFrom tippy renderTippy tippy_this
#' @importFrom shiny req reactive renderUI numericInput withMathJax
#' @importFrom shinyWidgets pickerInput
#' @export
#'
riskMeasuresBox <- function(input, output, session, law) {
    ns <- session$ns

    ####    Define distributions    ####
    law.fct <- ifelse(law == "exp", "gamma", law)
    parameters_latex <- dplyr::case_when(
        law %in% c("norm", "lnorm") ~ c("$$\\mu$$", "$$\\sigma$$"),
        law %in% c("gamma", "exp", "beta") ~ c("$$\\alpha$$", "$$\\beta$$"),
        law == "erlang" ~ c("$$n$$", "$$\\beta$$"),
        law == "unif" ~ c("$$a$$", "$$b$$"),
        law == "weibull" ~ c("$$\\tau$$", "$$\\beta$$"),
        law == "pareto" ~ c("$$\\alpha$$", "$$\\lambda$$"),
        law == "llogis" ~ c("$$\\lambda$$", "$$\\tau$$"),
        law == "IG" ~ c("$$\\mu$$", "$$\\beta$$"),
        # law == "burr" ~ c("$$\\alpha$$", "$$\\lambda$$", "$$\\tau$$"),
        TRUE ~ c("shape", "rate")
    )

    ####    Render LaTeX    ####
    output$VaR_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("VaR"),
            tooltip = paste0("$VaR_{\\kappa}(X) = $", dplyr::case_when(
                law == "norm" ~ "$\\mu + \\sigma \\Phi^{-1}(\\kappa)$",
                law == "lnorm" ~ "$\\mathrm{e}^{\\mu + \\sigma VaR_{\\kappa}\\left(Z\\right)}$",
                # law == "gamma" ~ "$$",
                law == "exp" ~ "$-\\frac{1}{\\beta} \\ln\\left(1 - \\kappa\\right)$",
                law == "llogis" ~ "$\\lambda \\left(\\kappa^{-1} - 1\\right)^{-1 / \\tau}$",
                law == "weibull" ~ "$\\frac{1}{\\beta}(-\\ln\\left(1 - \\kappa\\right))^{\\frac{1}{\\tau}}$",
                # law == "beta" ~ "$$",
                law == "unif" ~ "$a + \\left(b - a\\right) \\kappa$",
                law == "pareto" ~ "$\\lambda \\left(\\left(1 - \\kappa\\right)^{-\\frac{1}{\\alpha}} - 1\\right)$",
                TRUE ~ "VaR_{\\kappa}(X)"
            ))
        )
    })
    output$TVaR_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("TVaR"),
            tooltip = paste0("$TVaR_{\\kappa}(X) = $", dplyr::case_when(
                law == "norm" ~ "$\\mu + \\sigma TVaR_{\\kappa}(Z)$",
                law == "lnorm" ~ "$\\frac{1}{1 - \\kappa}\\mathrm{e}^{\\mu + \\sigma^{2} / 2} (1 - \\Phi(VaR_{\\kappa}(Z) - \\sigma))$",
                law == "gamma" ~ "$\\frac{1}{1 - \\kappa}\\frac{\\alpha}{\\beta} \\overline{H}\\left(VaR_{\\kappa}\\left(X\\right); \\alpha + 1, \\beta\\right)$",
                law == "exp" ~ "$VaR_{\\kappa}\\left(X\\right) + \\text{E}\\left[X\\right]$",
                law == "llogis" ~ "$\\frac{\\lambda}{1 - \\kappa}\\Gamma \\left(1 + \\frac{1}{\\tau }\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) \\overline{B}\\left(\\kappa; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right)$",
                law == "weibull" ~ "$\\frac{1}{\\beta (1 - \\kappa)}\\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\bar{\\text{H}}\\left(-\\ln(1 - \\kappa); 1 + \\frac{1}{\\tau}, 1\\right)$",
                law == "beta" ~ "$\\frac{1}{(1 - \\kappa)} \\frac{\\alpha}{\\alpha + \\beta} (1 - B(VaR_{\\kappa}(X); \\alpha + 1, \\beta))$",
                law == "unif" ~ "$a + \\frac{\\left(b - a\\right)}{2} \\left(1 + \\kappa\\right) $",
                law == "pareto" ~ "$\\lambda \\left(\\frac{\\alpha}{\\alpha - 1}\\left(1 - \\kappa\\right)^{-\\frac{1}{\\alpha}} - 1\\right)$",
                TRUE ~ "TVaR_{\\kappa}(X)"
            ))
        )
    })

    ####    Creates input (kap) ####
    kap <- shiny::reactive({
        input$kap
    })

    #### Creates input for the plots (which function to plot) ####
    plot_choice_QX <- shiny::reactive({
        input$plot_choice_QX
    })

    #### Render input (kap) ####
    output$kap <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("kap"),
            label = shiny::withMathJax("$$\\kappa$$"),
            value = 0.99,
            min = 0, max = 1, step = 0.10,
            width = "20px"
        )
    })

    #### Render input for the plots (which function to plot) ####
    output$plot_choice_QX <- shiny::renderUI({
        shinyWidgets::pickerInput(
            inputId = session$ns("plot_choice_QX"),
            # label = "Style : primary",
            choices = c(
                "Density Function",
                "Cumulative Density Function",
                "Quantile Function"
            ),
            selected = "Cumulative Density Function",
            options = list(
                style = "btn-success"
            )
        )
    })

    #### Calculate risk measures ####
    VaR <- shiny::reactive({
        format(
            rlang::exec(
                .fn = paste0("VaR_", law.fct),
                kap = as.numeric(kap()),
                as.numeric(input$shape), as.numeric(input$rate)
            ),
            nsmall = 6
        )
    })
    TVaR <- shiny::reactive({
        format(
            rlang::exec(
                .fn = paste0("TVaR_", law.fct),
                kap = as.numeric(kap()),
                as.numeric(input$shape), as.numeric(input$rate)
            ),
            nsmall = 6
        )
    })

    #### Render risk measures ####
    output$VaR <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                   # VaR_Quantile_LATEX(),
                                   kap(),
                                   VaR()
        )
        )
    })
    output$TVaR <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                   kap(),
                                   TVaR()
        )
        )
    })

    #### Render plots ####
    output$Qx <- plotly::renderPlotly({
        shiny::req(input$shape, input$rate, plot_choice_QX())
        if (plot_choice_QX() == "Quantile Function") {
            ggplot2::ggplot(data = data.frame(x.limz = c(0, 1)), ggplot2::aes_(x = ~x.limz)) +
                ggplot2::stat_function(
                    fun = function(xx) rlang::exec(
                        .fn = ifelse(law.fct %in% c("pareto", "llogis"), paste0("VaR_", law.fct), paste0("q", law.fct)),
                        xx,
                        as.numeric(input$shape), as.numeric(input$rate),
                        .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                    )
                ) +
                ggplot2::theme_classic() +
                ggplot2::labs(
                    x = "$$\\kappa$$",
                    y = "$$y$$"
                )
        } else {
            ggplot2::ggplot(data = data.frame(
                x.limz = c(rlang::exec(
                    .fn = paste0("VaR_", law),
                    kap = 0.01,
                    as.numeric(input$shape), as.numeric(input$rate),
                    .env = rlang::ns_env(x = "Distributacalcul")
                ), rlang::exec(
                    .fn = paste0("VaR_", law),
                    kap = 0.99,
                    as.numeric(input$shape), as.numeric(input$rate),
                    .env = rlang::ns_env(x = "Distributacalcul")
                ))), ggplot2::aes_(x = ~x.limz)) +
                ggplot2::stat_function(
                    fun = Vectorize(function(xx) rlang::exec(
                        .fn = paste0(ifelse(plot_choice_QX() == "Density Function", "d", "p"), law),
                        xx,
                        as.numeric(input$shape), as.numeric(input$rate),
                        .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                    )),
                    alpha = 0.7
                ) +
                ggplot2::stat_function(
                    fun = Vectorize(function(xx) rlang::exec(
                        .fn = paste0(ifelse(plot_choice_QX() == "Density Function",
                                            "d",
                                            "p")
                                     ,law
                        ),
                        xx,
                        as.numeric(input$shape), as.numeric(input$rate),
                        .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                    )),
                    xlim = c(VaR(),  rlang::exec(
                        .fn = paste0("VaR_", law),
                        kap = 0.99,
                        as.numeric(input$shape), as.numeric(input$rate),
                        .env = rlang::ns_env(x = "Distributacalcul")
                    )),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                ) +
                ggplot2::theme_classic()
        }
    })
}

#' Interactive risk measures visualization (UI side)
#'
#' @param id id of module
#'
#' @return UI function for the risk measures module.
#'  Should not be run directly.
#'
#' @importFrom shiny tags NS uiOutput
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom tippy tippyOutput
#' @importFrom plotly plotlyOutput
#' @export
#'
riskMeasuresBoxUI <- function(id) {
    ns <- shiny::NS(id)

    shinydashboardPlus::boxPlus(
        title = "Risk measures",
        width = NULL,
        solidHeader = TRUE,
        closable = FALSE,
        status = "success",
        shiny::uiOutput(ns("kap")),
        shiny::uiOutput(ns("VaR")),
        shiny::withMathJax(tippy::tippyOutput(ns("VaR_tip"))),
        shiny::uiOutput(ns("TVaR")),
        shiny::withMathJax(tippy::tippyOutput(ns("TVaR_tip"))),
        shiny::uiOutput(ns("plot_choice_QX")),
        plotly::plotlyOutput(ns("Qx"))
    )
}
