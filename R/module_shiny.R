#' Interactive distribution visualization (server side)
#'
#' @param input input for server side.
#' @param output output for server side.
#' @param session session for server side.
#' @param law Distribution to visualize, one of ...
#'
#' @return Server function for the \code{\link{Distributacalcul_vis}} module.
#'  Should not be run directly.
#'
#' @importFrom rlang exec ns_env
#' @importFrom dplyr case_when
#' @importFrom ggplot2 ggplot stat_function aes labs theme_classic
#' @importFrom plotly renderPlotly
#' @importFrom tippy renderTippy tippy_this
#' @importFrom shiny req reactive renderUI numericInput withMathJax
#' @importFrom shinyWidgets radioGroupButtons switchInput pickerInput
#' @export
#'
lawParametersBox <- function(input, output, session, law) {
    stopifnot(law %in% c("norm", "lnorm",
                         "gamma", "exp",
                         # "erlang",
                         "llogis", "weibull", "pareto",
                         "beta", "unif")
    )
    law.fct <- ifelse(law == "exp", "gamma", law)
    #### Define distributions ####
    # parameters_symbol_name <- list(
    #     case_when(
    #         law == "norm" ~ expr("mean"),
    #         law == "lnorm" ~ expr("meanlog"),
    #         law == "beta" ~ expr("shape1"),
    #         TRUE ~ expr("shape")
    #     ),
    #     case_when(
    #         law == "norm" ~ expr("sd"),
    #         law == "lnorm" ~ expr("sdlog"),
    #         law == "beta" ~ expr("shape2"),
    #         TRUE ~ expr("rate")
    #     )
    # )
    # parameters_distr <- list(
    #     as.numeric(shape()), as.numeric(rate())
    # )
    # names(parameters_distr) <- parameters_symbol_name

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
    ns <- session$ns

    #### Render LaTeX ####
    output$E_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("E"),
            tooltip = paste0("$\\text{E}[X] = $", dplyr::case_when(
                law == "norm" ~ "$\\mu$",
                law == "lnorm" ~ "$e^{\\mu + \\sigma^2 / 2}$",
                law == "gamma" ~ "$\\frac{\\alpha}{\\beta}$",
                law == "exp" ~ "$\\frac{1}{\\beta}$",
                law == "llogis" ~ "$\\lambda \\Gamma(1 + 1/\\tau) \\Gamma(1 - 1/\\tau)$",
                law == "weibull" ~ "$\\frac{\\Gamma(1 + 1 / \\tau)}{\\beta}$",
                law == "beta" ~ "$\\frac{\\alpha}{\\alpha + \\beta}$",
                law == "unif" ~ "$\\frac{a + b}{2}$",
                law == "pareto" ~ "$\\frac{\\lambda}{\\alpha - 1}$",
                TRUE ~ "\\text{E}[X]"
            ))
        )
    })
    output$V_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("V"),
            tooltip = paste0("$\\text{Var}(X) = $", dplyr::case_when(
                law == "norm" ~ "$\\sigma^2$",
                law == "lnorm" ~ "$e^{2\\mu + \\sigma^{2}}\\left(e^{\\sigma^{2}} - 1\\right)$",
                law == "gamma" ~ "$\\frac{\\alpha}{\\beta^{2}}$",
                law == "exp" ~ "$\\frac{1}{\\beta^{2}}$",
                law == "llogis" ~ "$\\lambda^{2} \\left(\\Gamma \\left(1 + \\frac{2}{\\tau}\\right) \\Gamma \\left(1 - \\frac{2}{\\tau}\\right) - \\left(\\Gamma \\left(1 + \\frac{1}{\\tau}\\right) \\Gamma \\left(1 - \\frac{1}{\\tau}\\right) \\right)^{2}\\right)$",
                law == "weibull" ~ "$\\frac{\\Gamma(1 + 2/\\tau)}{\\beta^2} - \\left(\\frac{\\Gamma(1 + 1/\\tau)}{\\beta} \\right)^2$",
                law == "beta" ~ "$\\frac{\\alpha\\beta}{\\left(\\alpha + \\beta \\right)^{2} \\left(\\alpha + \\beta + 1\\right)}$",
                law == "unif" ~ "$\\frac{(b - a)^{2}}{12}$",
                law == "pareto" ~ "$\\frac{\\alpha\\lambda^{2}}{\\left(\\alpha - 1\\right)^{2} \\left(\\alpha - 2\\right)}$",
                TRUE ~ "\\text{Var}(X)"
            ))
        )
    })
    output$Etrunc_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("Etrunc"),
            tooltip = paste0("$\\text{E}(X \\times 1_{\\{X \\leq d\\}}) = $", dplyr::case_when(
                law == "norm" ~ "$\\mu \\Phi \\left(\\frac{d - \\mu}{\\sigma}\\right) - \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}}$",
                law == "lnorm" ~ "$\\exp \\left(\\mu + \\sigma^{2} / 2 \\right) \\Phi \\left(\\frac{\\ln d - \\mu - \\sigma^{2}}{\\sigma}\\right)$",
                law == "gamma" ~ "$\\frac{\\alpha}{\\beta} H\\left(d; \\alpha + 1, \\beta \\right)$",
                law == "exp" ~ "$\\frac{1}{\\beta} \\left(1 - \\mathrm{e}^{-\\beta d}\\right) - d\\mathrm{e}^{-\\beta d}$",
                law == "llogis" ~ "$\\lambda \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) B\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right)$",
                law == "weibull" ~ "$\\frac{1}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) H(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau})$",
                law == "beta" ~ "$\\frac{\\alpha}{\\alpha + \\beta} B(d; \\alpha + 1, \\beta)$",
                law == "unif" ~ "$\\frac{d^{2} - a^{2}}{2(b - a)}$",
                law == "pareto" ~ "$\\frac{\\lambda}{\\alpha - 1} \\left(1 - \\frac{\\lambda^{\\alpha - 1}}{\\left(\\lambda + d\\right)^{\\alpha - 1}}\\right) - d\\left(\\frac{\\lambda}{\\lambda + d}\\right)^{\\alpha}$",
                TRUE ~ "\\text{E}(X \\times 1_{\\{X \\leq d\\}})"
            ))
        )
    })
    output$SL_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("SL"),
            tooltip = paste0("$\\pi_{X}(d) = $", dplyr::case_when(
                law == "norm" ~ "$(\\mu + d) \\bar\\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right) - \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}}$",
                law == "lnorm" ~ "$\\mathrm{e}^{\\mu + \\sigma^{2} / 2} \\left(1 - \\Phi\\left(\\frac{\\ln(d) - \\mu - \\sigma^{2}}{\\sigma}\\right)\\right) - d \\left[1 - \\Phi\\left(\\frac{\\ln d - \\mu}{\\sigma}\\right)\\right]$",
                law == "gamma" ~ "$\\frac{\\alpha}{\\beta} \\overline{H}\\left(d; \\alpha + 1, \\beta\\right) - d\\overline{H}\\left(d; \\alpha, \\beta\\right)$",
                law == "exp" ~ "$ \\frac{1}{\\beta} \\mathrm{e}^{-\\beta d}$",
                law == "llogis" ~ "$\\lambda \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) \\overline{B}\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right) - \\frac{d\\lambda^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}$",
                law == "weibull" ~ "$\\frac{1}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) \\overline{H}(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau}) - d\\mathrm{e}^{-(\\beta d)^{\\tau}}$",
                law == "beta" ~ "$\\frac{\\alpha}{\\alpha + \\beta} (1 - B(d; \\alpha + 1, \\beta)) - d(1 - B(d; \\alpha, \\beta))$",
                law == "unif" ~ "$\\frac{(b - d)^{2}}{2(b - a)}$",
                law == "pareto" ~ "$\\frac{\\lambda}{\\alpha - 1} \\left(\\frac{\\lambda}{\\lambda + d}\\right)^{\\alpha - 1}$",
                TRUE ~ "\\pi_{X}(d)"
            ))
        )
    })
    output$Elim_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("Elim"),
            tooltip = paste0("$\\text{E}[\\min(X; d)] = $", dplyr::case_when(
                law == "norm" ~ "$\\mu \\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right) - \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}} + d \\bar\\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right)$",
                law == "lnorm" ~ "$\\mathrm{e}^{\\mu + \\sigma^{2} /  2} \\Phi\\left(\\frac{\\ln d - \\mu - \\sigma^{2}}{\\sigma}\\right) + d\\left[1 - \\Phi\\left(\\frac{\\ln d - \\mu}{\\sigma}\\right)\\right]$",
                law == "gamma" ~ "$\\frac{\\alpha}{\\beta} H\\left(d; \\alpha + 1, \\beta\\right) + d\\overline{H}\\left(d; \\alpha, \\beta\\right)$",
                law == "exp" ~ "$\\frac{1}{\\beta}\\left(1 - \\mathrm{e}^{-\\beta d}\\right)$",
                law == "llogis" ~ "$\\lambda \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) B\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right) + \\frac{d\\lambda^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}$",
                law == "weibull" ~ "$\\frac{1}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) H(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau}) + d\\mathrm{e}^{-(\\beta d)^{\\tau}}$",
                law == "beta" ~ "$\\frac{\\alpha}{\\alpha + 1}\\frac{1 - d^{\\alpha + 1}}{1 - d^{\\alpha}} -  d$",
                law == "unif" ~ "$\\frac{d^{2} - a^{2}}{2(b - a)} + d\\frac{b - d}{b - a}$",
                law == "pareto" ~ "$\\frac{\\lambda}{\\alpha - 1} \\left[1 - (\\frac{\\lambda}{\\lambda + d})^{\\alpha - 1}\\right]$",
                TRUE ~ "\\text{E}[\\min(X; d)]"
            ))
        )
    })
    output$Mexcess_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("Mexcess"),
            tooltip = paste0("$e_{X}(d) = $", dplyr::case_when(
                law == "norm" ~ "$(\\mu + d) - \\frac{1}{\\bar\\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right)} \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}}$",
                law == "lnorm" ~ "$\\frac{1}{\\left[1 - \\Phi\\left(\\frac{\\ln(d) - \\mu}{\\sigma}\\right)\\right]} \\mathrm{e}^{\\mu + \\sigma^{2} / 2} \\left(1 - \\Phi\\left(\\frac{\\ln d - \\mu -\\sigma^{2}}{\\sigma}\\right)\\right) - d$",
                law == "gamma" ~ "$\\frac{\\alpha}{\\beta} \\frac{\\overline{H}\\left(d; \\alpha + 1, \\beta\\right)}{\\overline{H}\\left(d; \\alpha, \\beta\\right)} - d$",
                law == "exp" ~ "$\\frac{1}{\\beta}$",
                law == "llogis" ~ "$\\frac{\\lambda^{\\tau} + d^{\\tau}}{\\lambda^{\\tau - 1}} \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) \\overline{B}\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right) - d$",
                law == "weibull" ~ "$\\frac{e^{(\\beta d)^{\\tau}}}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) \\overline{H}(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau}) - d$",
                law == "beta" ~ "$\\frac{\\alpha}{\\alpha + 1}\\frac{1 - d^{\\alpha + 1}}{1 - d^{\\alpha}} - d$",
                law == "unif" ~ "$\\frac{b - d}{2}$",
                law == "pareto" ~ "$\\frac{\\lambda + d}{\\alpha - 1}$",
                TRUE ~ "e_{X}(d)"
            ))
        )
    })
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
    output$density_distr_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("density_distr"),
            tooltip = paste0("$f(x) = $", dplyr::case_when(
                law == "norm" ~ "$\\frac{{e}^{-\\frac{(x - \\mu)^{2}}{2 \\sigma^{2}}}}{\\sqrt{2\\pi}\\sigma}$",
                law == "lnorm" ~ "$\\frac{1}{x \\sqrt{2\\pi} \\sigma} \\mathrm{e}^{-\\frac{(\\ln(x) - \\mu )^{2}}{2 \\sigma^{2}}}$",
                law == "gamma" ~ "$\\frac{\\beta^{\\alpha}}{\\Gamma (\\alpha)} x^{\\alpha - 1} \\mathrm{e}^{-\\beta x}$",
                law == "exp" ~ "$\\beta \\mathrm{e}^{-\\beta x}$",
                law == "llogis" ~ "$\\frac{\\tau \\lambda^\\tau x^{\\tau - 1}}{(\\lambda^{\\tau} + x^{\\tau})^{2}}$",
                law == "weibull" ~ "$\\beta \\tau \\left(\\beta x\\right)^{\\tau - 1} \\mathrm{e}^{-\\left(\\beta x\\right)^{\\tau}}$",
                law == "beta" ~ "$\\frac{x^{\\alpha -1}\\left(1 - x\\right)^{\\beta - 1}}{I\\left(\\alpha, \\beta\\right)} \\times 1_{\\left\\{x \\in \\left[0, 1\\right]\\right\\}}$",
                law == "unif" ~ "$\\frac{1}{b - a}\\times 1_{\\left\\{x \\in \\left[a, b\\right]\\right\\} }$",
                law == "pareto" ~ "$\\frac{\\alpha \\lambda^{\\alpha}}{\\left(\\lambda + x\\right)^{\\alpha + 1}}$",
                TRUE ~ "f(x)"
            ))
        )
    })
    output$repartSurvie_distr_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("repartSurvie_distr"),
            tooltip = paste0("$F_{X}(x) = $", dplyr::case_when(
                law == "norm" ~ "$\\Phi\\left(\\frac{x - \\mu}{\\sigma}\\right)$",
                law == "lnorm" ~ "$\\Phi\\left(\\frac{\\ln(x) - \\mu}{\\sigma}\\right)$",
                law == "gamma" ~ "$H\\left(x; \\alpha, \\beta \\right)$",
                law == "exp" ~ "$1 - \\mathrm{e}^{-\\beta x}$",
                law == "llogis" ~ "$1 - \\frac{\\lambda^{\\tau}}{\\lambda^\\tau + x^\\tau}$",
                law == "weibull" ~ "$1 - \\mathrm{e}^{-\\left(\\beta x\\right)^{\\tau}}$",
                law == "beta" ~ "$B\\left(x; \\alpha, \\beta\\right)$",
                # law == "unif" ~ "$\\left\\{\\begin{array}{ll}
                # 0, & x<a \\\\
                # \\frac{x-a}{b-a}, & a\\leq x\\leq b \\\\
                # 1, & x>b%
                # \\end{array}$",
                law == "pareto" ~ "$1 - \\left(\\frac{\\lambda}{\\lambda + x}\\right)^{\\alpha}$",
                TRUE ~ "F_{X}(x)"
            ))
        )
    })

    #### Creates parameters (shape, rate) ####
    shape <- shiny::reactive({
        input$shape
    })
    rate <- shiny::reactive({
        input$rate
    })

    #### Creates input (d, less.than.d, x, kap ...) ####
    d <- shiny::reactive({
        input$d
    })
    less.than.d <- shiny::reactive({
        input$less.than.d
    })

    kap <- shiny::reactive({
        input$kap
    })

    x <- shiny::reactive({
        input$x
    })
    xlim_distr <- shiny::reactive({
        input$xlim_distr
    })

    #### Creates input for the plots (which function to plot) ####
    plot_choice_QX <- shiny::reactive({
        input$plot_choice_QX
    })
    plot_choice_FSX <- shiny::reactive({
        input$plot_choice_FSX
    })

    #### Render parameters (shape, rate) ####
    output$shape <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("shape"),
            label = shiny::withMathJax(parameters_latex[1]),
            value = dplyr::case_when(
                law.fct %in% c("pareto", "llogis") ~ 3,
                TRUE ~ 2
            ),
            min = dplyr::case_when(
                law.fct %in% c("lnorm", "gamma", "exp", "beta") ~ 0
            )
        )
    })
    output$rate <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("rate"),
            label = shiny::withMathJax(parameters_latex[2]),
            value = 3,
            min = dplyr::case_when(
                law.fct %in% c("lnorm", "norm", "gamma", "beta") ~ 0
            )
        )
    })

    #### Render input (d, less.than.d, x, kap ...) ####
    output$d <- shiny::renderUI({
        shiny::req(shape(), rate())
        shiny::numericInput(
            inputId = session$ns("d"),
            label = shiny::withMathJax("$$d$$"),
            value = dplyr::case_when(
                law.fct == "unif" ~ 2,
                TRUE ~ 1
            ),
            min = dplyr::case_when(
                law.fct %in% c("lnorm", "gamma", "exp", "beta") ~ 0,
                law.fct == c("unif") ~ as.numeric(shape())
            ),
            max = dplyr::case_when(
                law.fct == "beta" ~ 1,
                law.fct == "unif" ~ as.numeric(rate())
            ),
            width = "20px"
        )
    })
    output$less.than.d <- shiny::renderUI({
        shinyWidgets::radioGroupButtons(
            inputId = session$ns("less.than.d"),
            label = "",
            choiceNames = list(shiny::withMathJax("$\\geq$"), shiny::withMathJax("$\\leq$")),
            choiceValues = as.logical(list(TRUE, FALSE))
        )
    })

    output$kap <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("kap"),
            label = shiny::withMathJax("$$\\kappa$$"),
            value = 0.99,
            min = 0, max = 1, step = 0.10,
            width = "20px"
        )
    })

    output$x <- shiny::renderUI({
        shiny::req(shape(), rate())
        shiny::numericInput(
            inputId = session$ns("x"),
            label = shiny::withMathJax("$$x$$"),
            value = 2,
            min = dplyr::case_when(
                law.fct %in% c("lnorm", "gamma", "exp", "beta") ~ 0,
                law.fct == c("unif") ~ as.numeric(shape())
            ),
            max = dplyr::case_when(
                law.fct == "beta" ~ 1,
                law.fct == "unif" ~ as.numeric(rate())
            ),
            width = "20px"
        )
    })
    output$xlim_distr <- shiny::renderUI({
        shinyWidgets::switchInput(
            inputId = session$ns("xlim_distr"),
            onStatus = "success",
            onLabel = "Repartition",
            offStatus = "info",
            offLabel = "Survie",
            value = TRUE,
            labelWidth = "10px"
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
    output$plot_choice_FSX <- shiny::renderUI({
        shinyWidgets::pickerInput(
            inputId = session$ns("plot_choice_FSX"),
            choices = c(
                "Density Function",
                "Cumulative Density Function"
            ),
            selected = "Density Function",
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
                as.numeric(shape()), as.numeric(rate())
            ),
            nsmall = 6
        )
    })
    TVaR <- shiny::reactive({
        format(
            rlang::exec(
                .fn = paste0("TVaR_", law.fct),
                kap = as.numeric(kap()),
                as.numeric(shape()), as.numeric(rate())
            ),
            nsmall = 6
        )
    })

    #### Calculate functions ####
    density_distr <- shiny::reactive({
        format(
            rlang::exec(
                .fn = paste0("d", law.fct),
                x = as.numeric(x()),
                as.numeric(shape()), as.numeric(rate()),
                .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
            ),
            nsmall = 6
        )
    })
    repartSurvie_distr <- shiny::reactive({
        format(
            rlang::exec(
                .fn = paste0("p", law.fct),
                q = as.numeric(x()),
                as.numeric(shape()), as.numeric(rate()),
                lower.tail = xlim_distr(),
                .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
            ),
            nsmall = 6,
            scientific = FALSE
        )
    })

    #### Calculate moments ####
    E <- shiny::reactive({
        rlang::exec(
            .fn = paste0("E_", law.fct),
            as.numeric(shape()), as.numeric(rate())
        )
    })
    V <- shiny::reactive({
        rlang::exec(
            .fn = paste0("V_", law.fct),
            as.numeric(shape()), as.numeric(rate())
        )
    })
    Etrunc <- shiny::reactive({
        shiny::req(less.than.d())
        rlang::exec(
            .fn = paste0("Etrunc_", law.fct),
            d = as.numeric(d()),
            as.numeric(shape()), as.numeric(rate()),
            less.than.d = ifelse(less.than.d(), TRUE, FALSE) ### here
        )
    })
    SL <- shiny::reactive({
        rlang::exec(
            .fn = paste0("SL_", law.fct),
            d = as.numeric(d()),
            as.numeric(shape()), as.numeric(rate())
        )
    })
    Elim <- shiny::reactive({
        rlang::exec(
            .fn = paste0("Elim_", law.fct),
            d = as.numeric(d()),
            as.numeric(shape()), as.numeric(rate())
        )
    })
    Mexcess <- shiny::reactive({
        rlang::exec(
            .fn = paste0("Mexcess_", law.fct),
            d = as.numeric(d()),
            as.numeric(shape()), as.numeric(rate())
        )
    })

    #### Render moments ####
    output$E <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$\\text{E}[X] = %s$$",
                                   E()
        )
        )
    })
    output$V <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$\\text{Var}(X) = %s$$",
                                   V()
        )
        )
    })
    output$Etrunc <- shiny::renderUI({
        shiny::withMathJax(
            sprintf(
                "$$\\text{E}[X \\times 1_{\\{X %s %s\\}}] = %.4f$$",
                ifelse(less.than.d(), "\\geq", "\\leq"),
                as.numeric(d()),
                Etrunc()
            )
        )
    })
    output$SL <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$\\pi_{%s}(X) = %.4f$$",
                                   d(),
                                   SL()
        )
        )
    })
    output$Elim <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$\\text{E}[\\min(X;%s)] = %.4f$$",
                                   d(),
                                   Elim()
        )
        )
    })
    output$Mexcess <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                   d(),
                                   Mexcess()))
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

    #### Render functions ####
    output$density_distr <- shiny::renderUI({
        shiny::withMathJax(sprintf("$$f(%s) = %s$$",
                                   as.numeric(x()),
                                   density_distr()
        )
        )
    })
    output$repartSurvie_distr <- shiny::renderUI({
        shiny::withMathJax(
            sprintf("$$%s(%s) = %s$$",
                    ifelse(xlim_distr(), "F_X{}", "S_{X}"),
                    as.numeric(x()),
                    repartSurvie_distr()
            )
        )
    })

    #### Render plots ####
    output$Qx <- plotly::renderPlotly({
        shiny::req(shape(), rate(), plot_choice_QX())
        if (plot_choice_QX() == "Quantile Function") {
            ggplot2::ggplot(data = data.frame(x.limz = c(0, 1)), ggplot2::aes_(x = ~x.limz)) +
                ggplot2::stat_function(
                    fun = function(xx) rlang::exec(
                        .fn = ifelse(law.fct %in% c("pareto", "llogis"), paste0("VaR_", law.fct), paste0("q", law.fct)),
                        xx,
                        as.numeric(shape()), as.numeric(rate()),
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
                    as.numeric(shape()), as.numeric(rate()),
                    .env = rlang::ns_env(x = "Distributacalcul")
                ), rlang::exec(
                    .fn = paste0("VaR_", law),
                    kap = 0.99,
                    as.numeric(shape()), as.numeric(rate()),
                    .env = rlang::ns_env(x = "Distributacalcul")
                ))), ggplot2::aes_(x = ~x.limz)) +
                ggplot2::stat_function(
                    fun = Vectorize(function(xx) rlang::exec(
                        .fn = paste0(ifelse(plot_choice_QX() == "Density Function", "d", "p"), law),
                        xx,
                        as.numeric(shape()), as.numeric(rate()),
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
                        as.numeric(shape()), as.numeric(rate()),
                        .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                    )),
                    xlim = c(VaR(),  rlang::exec(
                        .fn = paste0("VaR_", law),
                        kap = 0.99,
                        as.numeric(shape()), as.numeric(rate()),
                        .env = rlang::ns_env(x = "Distributacalcul")
                    )),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                ) +
                ggplot2::theme_classic()
        }
    })
    output$FSx <- plotly::renderPlotly({
        shiny::req(shape(), rate(), x(), plot_choice_FSX())
        ggplot2::ggplot(data = data.frame(
            x.limz = c(rlang::exec(
                .fn = paste0("VaR_", law),
                kap = 0.01,
                as.numeric(shape()), as.numeric(rate())
            ), rlang::exec(
                .fn = paste0("VaR_", law),
                kap = 0.99,
                as.numeric(shape()), as.numeric(rate())
            ))), ggplot2::aes_(x = ~x.limz)) +
            ggplot2::stat_function(
                fun = Vectorize(function(xx) rlang::exec(
                    .fn = paste0(ifelse(plot_choice_FSX() == "Density Function", "d", "p"), law),
                    xx,
                    as.numeric(shape()), as.numeric(rate()),
                    .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                )),
                alpha = 0.7
            ) +
            ggplot2::stat_function(
                fun = Vectorize(function(xx) rlang::exec(
                    .fn = paste0(ifelse(plot_choice_FSX() == "Density Function", "d", "p"), law),
                    xx,
                    as.numeric(shape()), as.numeric(rate()),
                    .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                )),
                xlim = c(
                    ifelse(xlim_distr(), rlang::exec(
                        .fn = paste0("VaR_", law),
                        kap = 0.01,
                        as.numeric(shape()), as.numeric(rate())
                    ), x()),
                    ifelse(xlim_distr(),
                           x(),
                           rlang::exec(
                               .fn = paste0("VaR_", law),
                               kap = 0.99,
                               as.numeric(shape()), as.numeric(rate())
                           )
                    )
                ),
                geom = "area",
                fill = ifelse(xlim_distr(), "Dark Green", "Royal Blue"),
                alpha = 0.7
            ) +
            ggplot2::theme_classic()
    })
}

#' Interactive distribution visualization (UI side)
#'
#' @param id id of module
#'
#' @return UI function for the \code{\link{Distributacalcul_vis}} module.
#'  Should not be run directly.
#'
#' @importFrom shiny tags NS fluidRow column uiOutput splitLayout
#' @importFrom shinydashboard box
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom tippy tippyOutput
#' @importFrom plotly plotlyOutput
#' @export
#'
lawParametersBoxUI <- function(id) {

    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::column(
            width = 3,

            #### Parameters ####
            shinydashboardPlus::boxPlus(
                title = "Parameters",
                status = "primary",
                background = "blue",
                solidHeader = TRUE,
                width = NULL,
                closable = FALSE,
                shiny::splitLayout(
                    shiny::uiOutput(ns("shape")),
                    shiny::uiOutput(ns("rate"))
                )
            ),
            shiny::tags$head(
                shiny::tags$style(
                    type = "text/css",
                    "label {
                    display: table-cell;
                    text-align: center;
                    vertical-align: middle;
                    }
                    .form-group {
                    display: table-row;
                    }
                    "
                )
                ),

            #### Moments ####
            shinydashboard::box(
                title = "Moments",
                width = NULL,
                solidHeader = TRUE,
                status = "warning",
                shiny::uiOutput(ns("E")),
                shiny::withMathJax(tippy::tippyOutput(ns("E_tip"))),
                shiny::uiOutput(ns("V")),
                shiny::withMathJax(tippy::tippyOutput(ns("V_tip"))),
                shiny::uiOutput(ns("d")),
                shiny::splitLayout(
                    shiny::uiOutput(ns("less.than.d")),
                    shiny::uiOutput(ns("Etrunc")),
                    shiny::withMathJax(tippy::tippyOutput(ns("Etrunc_tip"))),
                    cellWidths = 'auto'
                ),
                shiny::uiOutput(ns("Elim")),
                shiny::withMathJax(tippy::tippyOutput(ns("Elim_tip"))),
                shiny::uiOutput(ns("SL")),
                shiny::withMathJax(tippy::tippyOutput(ns("SL_tip"))),
                shiny::uiOutput(ns("Mexcess")),
                shiny::withMathJax(tippy::tippyOutput(ns("Mexcess_tip")))
            )
                ),

        #### Risk measures ####
        shiny::column(
            width = 4,
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
            ),
            align = "center"
        ),

        #### Functions ####
        shiny::column(
            width = 4,
            shinydashboardPlus::boxPlus(
                title = "Functions",
                width = NULL,
                solidHeader = TRUE,
                closable = FALSE,
                status = "danger",
                shiny::uiOutput(ns("x")),
                shiny::uiOutput(ns("density_distr")),
                shiny::withMathJax(tippy::tippyOutput(ns("density_distr_tip"))),
                shiny::uiOutput(ns("xlim_distr")),
                shiny::uiOutput(ns("repartSurvie_distr")),
                shiny::withMathJax(tippy::tippyOutput(ns("repartSurvie_distr_tip"))),
                shiny::uiOutput(ns("plot_choice_FSX")),
                plotly::plotlyOutput(ns("FSx"))
            ),
            align = "center"
        )
            )
    }

#' Interactive distribution visualization
#'
#' @description Opens an interactive Shiny app for the selected distribution.
#'
#' @param law Distribution to visualize, presently one of these 2 parameter
#'  continuous distributions :
#' \itemize{
#'  \item{"norm": }{Normal distribution.}
#'  \item{"lnorm": }{Lognormal distribution.}
#'  \item{"gamma": }{Gamma distribution.}
#  \item{"exp": }{Exponential distribution.}
#'  \item{"beta": }{Beta distribution.}
#'  \item{"unif": }{Uniform distribution.}
#'  \item{"llogis": }{Log-logistic distribution.}
#'  \item{"weibull": }{Weibull distribution.}
#'  \item{"pareto": }{Pareto distribution.}
#' }
#'
#' @importFrom shiny shinyApp callModule
#' @importFrom shinydashboardPlus dashboardPagePlus dashboardHeaderPlus
#' @importFrom shinydashboard dashboardSidebar dashboardBody
#' @export
#'
#' @return Launches Shiny application.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'    Distributacalcul_vis("norm")
#' }
#'
Distributacalcul_vis <- function(law) {
    .Deprecated(new = 'distributacalculVis', package = 'Distributacalcul', msg = "Deprecated. Use new and improved distributacalculVis instead.")
    shiny::shinyApp(
        ui = shinydashboardPlus::dashboardPagePlus(
            header = shinydashboardPlus::dashboardHeaderPlus(title = law),
            sidebar = shinydashboard::dashboardSidebar(width = NULL, collapsed = TRUE, disable = TRUE),
            body = shinydashboard::dashboardBody(lawParametersBoxUI(toupper(law)))
        ),
        server = function(input, output, session) {
            shiny::callModule(
                module = lawParametersBox,
                id = toupper(law),
                law = law
            )
        }
    )
}
