#' Interactive moments visualization (server side)
#'
#' @template input-template
#' @template output-template
#' @template session-template
#' @param law Distribution to visualize, one of ...
#' @template lang-template
#'
#' @return Server function for the moments module.
#'  Should not be run directly.
#'
#' @importFrom rlang exec
#' @importFrom dplyr case_when
#' @importFrom tippy renderTippy tippy_this
#' @importFrom shiny req reactive renderUI numericInput withMathJax
#' @importFrom shinyWidgets radioGroupButtons
#' @export
#'
#' @keywords internal
#'
momentsBox <- function(input, output, session, law, lang) {
    ns <- session$ns

    ####    Define distributions    ####
    law.fct <- ifelse(law == "Exp", "Gamma", law)
    parameters_latex <- dplyr::case_when(
        law %in% c("Norm", "Lnorm") ~ c("$$\\mu$$", "$$\\sigma$$"),
        law %in% c("Gamma", "Exp", "Beta") ~ c("$$\\alpha$$", "$$\\beta$$"),
        law == "Erlang" ~ c("$$n$$", "$$\\beta$$"),
        law == "Unif" ~ c("$$a$$", "$$b$$"),
        law == "Weibull" ~ c("$$\\tau$$", "$$\\beta$$"),
        law == "Pareto" ~ c("$$\\alpha$$", "$$\\lambda$$"),
        law == "Llogis" ~ c("$$\\lambda$$", "$$\\tau$$"),
        law == "IG" ~ c("$$\\mu$$", "$$\\beta$$"),
        # law == "burr" ~ c("$$\\alpha$$", "$$\\lambda$$", "$$\\tau$$"),
        TRUE ~ c("shape", "rate")
    )

    ####    Render LaTeX    ####
    output$E_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("E"),
            tooltip = paste0("$\\text{E}[X] = $", dplyr::case_when(
                law == "Norm" ~ "$\\mu$",
                law == "Lnorm" ~ "$e^{\\mu + \\sigma^2 / 2}$",
                law == "Gamma" ~ "$\\frac{\\alpha}{\\beta}$",
                law == "Exp" ~ "$\\frac{1}{\\beta}$",
                law == "Llogis" ~ "$\\lambda \\Gamma(1 + 1/\\tau) \\Gamma(1 - 1/\\tau)$",
                law == "Weibull" ~ "$\\frac{\\Gamma(1 + 1 / \\tau)}{\\beta}$",
                law == "Beta" ~ "$\\frac{\\alpha}{\\alpha + \\beta}$",
                law == "Unif" ~ "$\\frac{a + b}{2}$",
                law == "Pareto" ~ "$\\frac{\\lambda}{\\alpha - 1}$",
                TRUE ~ "\\text{E}[X]"
            ))
        )
    })
    output$V_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("V"),
            tooltip = paste0("$\\text{Var}(X) = $", dplyr::case_when(
                law == "Norm" ~ "$\\sigma^2$",
                law == "Lnorm" ~ "$e^{2\\mu + \\sigma^{2}}\\left(e^{\\sigma^{2}} - 1\\right)$",
                law == "Gamma" ~ "$\\frac{\\alpha}{\\beta^{2}}$",
                law == "Exp" ~ "$\\frac{1}{\\beta^{2}}$",
                law == "Llogis" ~ "$\\lambda^{2} \\left(\\Gamma \\left(1 + \\frac{2}{\\tau}\\right) \\Gamma \\left(1 - \\frac{2}{\\tau}\\right) - \\left(\\Gamma \\left(1 + \\frac{1}{\\tau}\\right) \\Gamma \\left(1 - \\frac{1}{\\tau}\\right) \\right)^{2}\\right)$",
                law == "Weibull" ~ "$\\frac{\\Gamma(1 + 2/\\tau)}{\\beta^2} - \\left(\\frac{\\Gamma(1 + 1/\\tau)}{\\beta} \\right)^2$",
                law == "Beta" ~ "$\\frac{\\alpha\\beta}{\\left(\\alpha + \\beta \\right)^{2} \\left(\\alpha + \\beta + 1\\right)}$",
                law == "Unif" ~ "$\\frac{(b - a)^{2}}{12}$",
                law == "Pareto" ~ "$\\frac{\\alpha\\lambda^{2}}{\\left(\\alpha - 1\\right)^{2} \\left(\\alpha - 2\\right)}$",
                TRUE ~ "\\text{Var}(X)"
            ))
        )
    })
    output$Etrunc_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("Etrunc"),
            tooltip = paste0("$\\text{E}(X \\times 1_{\\{X \\leq d\\}}) = $", dplyr::case_when(
                law == "Norm" ~ "$\\mu \\Phi \\left(\\frac{d - \\mu}{\\sigma}\\right) - \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}}$",
                law == "Lnorm" ~ "$\\exp \\left(\\mu + \\sigma^{2} / 2 \\right) \\Phi \\left(\\frac{\\ln d - \\mu - \\sigma^{2}}{\\sigma}\\right)$",
                law == "Gamma" ~ "$\\frac{\\alpha}{\\beta} H\\left(d; \\alpha + 1, \\beta \\right)$",
                law == "Exp" ~ "$\\frac{1}{\\beta} \\left(1 - \\mathrm{e}^{-\\beta d}\\right) - d\\mathrm{e}^{-\\beta d}$",
                law == "Llogis" ~ "$\\lambda \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) B\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right)$",
                law == "Weibull" ~ "$\\frac{1}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) H(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau})$",
                law == "Beta" ~ "$\\frac{\\alpha}{\\alpha + \\beta} B(d; \\alpha + 1, \\beta)$",
                law == "Unif" ~ "$\\frac{d^{2} - a^{2}}{2(b - a)}$",
                law == "Pareto" ~ "$\\frac{\\lambda}{\\alpha - 1} \\left(1 - \\frac{\\lambda^{\\alpha - 1}}{\\left(\\lambda + d\\right)^{\\alpha - 1}}\\right) - d\\left(\\frac{\\lambda}{\\lambda + d}\\right)^{\\alpha}$",
                TRUE ~ "\\text{E}(X \\times 1_{\\{X \\leq d\\}})"
            ))
        )
    })
    output$SL_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("SL"),
            tooltip = paste0("$\\pi_{X}(d) = $", dplyr::case_when(
                law == "Norm" ~ "$(\\mu + d) \\bar\\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right) - \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}}$",
                law == "Lnorm" ~ "$\\mathrm{e}^{\\mu + \\sigma^{2} / 2} \\left(1 - \\Phi\\left(\\frac{\\ln(d) - \\mu - \\sigma^{2}}{\\sigma}\\right)\\right) - d \\left[1 - \\Phi\\left(\\frac{\\ln d - \\mu}{\\sigma}\\right)\\right]$",
                law == "Gamma" ~ "$\\frac{\\alpha}{\\beta} \\overline{H}\\left(d; \\alpha + 1, \\beta\\right) - d\\overline{H}\\left(d; \\alpha, \\beta\\right)$",
                law == "Exp" ~ "$ \\frac{1}{\\beta} \\mathrm{e}^{-\\beta d}$",
                law == "Llogis" ~ "$\\lambda \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) \\overline{B}\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right) - \\frac{d\\lambda^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}$",
                law == "Weibull" ~ "$\\frac{1}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) \\overline{H}(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau}) - d\\mathrm{e}^{-(\\beta d)^{\\tau}}$",
                law == "Beta" ~ "$\\frac{\\alpha}{\\alpha + \\beta} (1 - B(d; \\alpha + 1, \\beta)) - d(1 - B(d; \\alpha, \\beta))$",
                law == "Unif" ~ "$\\frac{(b - d)^{2}}{2(b - a)}$",
                law == "Pareto" ~ "$\\frac{\\lambda}{\\alpha - 1} \\left(\\frac{\\lambda}{\\lambda + d}\\right)^{\\alpha - 1}$",
                TRUE ~ "\\pi_{X}(d)"
            ))
        )
    })
    output$Elim_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("Elim"),
            tooltip = paste0("$\\text{E}[\\min(X; d)] = $", dplyr::case_when(
                law == "Norm" ~ "$\\mu \\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right) - \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}} + d \\bar\\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right)$",
                law == "Lnorm" ~ "$\\mathrm{e}^{\\mu + \\sigma^{2} /  2} \\Phi\\left(\\frac{\\ln d - \\mu - \\sigma^{2}}{\\sigma}\\right) + d\\left[1 - \\Phi\\left(\\frac{\\ln d - \\mu}{\\sigma}\\right)\\right]$",
                law == "Gamma" ~ "$\\frac{\\alpha}{\\beta} H\\left(d; \\alpha + 1, \\beta\\right) + d\\overline{H}\\left(d; \\alpha, \\beta\\right)$",
                law == "Exp" ~ "$\\frac{1}{\\beta}\\left(1 - \\mathrm{e}^{-\\beta d}\\right)$",
                law == "Llogis" ~ "$\\lambda \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) B\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right) + \\frac{d\\lambda^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}$",
                law == "Weibull" ~ "$\\frac{1}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) H(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau}) + d\\mathrm{e}^{-(\\beta d)^{\\tau}}$",
                law == "Beta" ~ "$\\frac{\\alpha}{\\alpha + 1}\\frac{1 - d^{\\alpha + 1}}{1 - d^{\\alpha}} -  d$",
                law == "Unif" ~ "$\\frac{d^{2} - a^{2}}{2(b - a)} + d\\frac{b - d}{b - a}$",
                law == "Pareto" ~ "$\\frac{\\lambda}{\\alpha - 1} \\left[1 - (\\frac{\\lambda}{\\lambda + d})^{\\alpha - 1}\\right]$",
                TRUE ~ "\\text{E}[\\min(X; d)]"
            ))
        )
    })
    output$Mexcess_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("Mexcess"),
            tooltip = paste0("$e_{X}(d) = $", dplyr::case_when(
                law == "Norm" ~ "$(\\mu + d) - \\frac{1}{\\bar\\Phi\\left(\\frac{d - \\mu}{\\sigma}\\right)} \\sigma \\frac{e^{\\frac{-(d - \\mu)^2}{2 \\sigma^2}}}{\\sqrt{2\\pi}}$",
                law == "Lnorm" ~ "$\\frac{1}{\\left[1 - \\Phi\\left(\\frac{\\ln(d) - \\mu}{\\sigma}\\right)\\right]} \\mathrm{e}^{\\mu + \\sigma^{2} / 2} \\left(1 - \\Phi\\left(\\frac{\\ln d - \\mu -\\sigma^{2}}{\\sigma}\\right)\\right) - d$",
                law == "Gamma" ~ "$\\frac{\\alpha}{\\beta} \\frac{\\overline{H}\\left(d; \\alpha + 1, \\beta\\right)}{\\overline{H}\\left(d; \\alpha, \\beta\\right)} - d$",
                law == "Exp" ~ "$\\frac{1}{\\beta}$",
                law == "Llogis" ~ "$\\frac{\\lambda^{\\tau} + d^{\\tau}}{\\lambda^{\\tau - 1}} \\Gamma\\left(1 + \\frac{1}{\\tau}\\right) \\Gamma\\left(1 - \\frac{1}{\\tau}\\right) \\overline{B}\\left(\\frac{d^{\\tau}}{\\lambda^{\\tau} + d^{\\tau}}; 1 + \\frac{1}{\\tau}, 1 - \\frac{1}{\\tau}\\right) - d$",
                law == "Weibull" ~ "$\\frac{e^{(\\beta d)^{\\tau}}}{\\beta} \\Gamma(1 + \\frac{1}{\\tau}) \\overline{H}(d^{\\tau}; 1 + \\frac{1}{\\tau}, \\beta^{\\tau}) - d$",
                law == "Beta" ~ "$\\frac{\\alpha}{\\alpha + 1}\\frac{1 - d^{\\alpha + 1}}{1 - d^{\\alpha}} - d$",
                law == "Unif" ~ "$\\frac{b - d}{2}$",
                law == "Pareto" ~ "$\\frac{\\lambda + d}{\\alpha - 1}$",
                TRUE ~ "e_{X}(d)"
            ))
        )
    })

    ####    Creates input (d, less.than.d) ####
    d <- shiny::reactive({
        input$d
    })
    less.than.d <- shiny::reactive({
        input$less.than.d
    })

    #### Render input (d, less.than.d) ####
    output$d <- shiny::renderUI({
        shiny::req(input$shape, input$rate)
        shiny::numericInput(
            inputId = session$ns("d"),
            label = shiny::withMathJax("$$d$$"),
            value = dplyr::case_when(
                law.fct == "Unif" ~ 2,
                TRUE ~ 1
            ),
            min = dplyr::case_when(
                law.fct %in% c("Lnorm", "Gamma", "Exp", "Beta") ~ 0,
                law.fct == c("Unif") ~ as.numeric(input$shape)
            ),
            max = dplyr::case_when(
                law.fct == "Beta" ~ 1,
                law.fct == "Unif" ~ as.numeric(input$rate)
            ),
            width = "20px"
        )
    })
    output$less.than.d <- shiny::renderUI({
        shinyWidgets::radioGroupButtons(
            inputId = session$ns("less.than.d"),
            label = "",
            choiceNames = list("$\\geq$", "$\\leq$"),
            choiceValues = as.logical(list(TRUE, FALSE))
        )
    })

    #### Calculate moments ####
    E <- shiny::reactive({
        rlang::exec(
            .fn = paste0("expVal", law.fct),
            as.numeric(input$shape), as.numeric(input$rate)
        )
    })
    V <- shiny::reactive({
        rlang::exec(
            .fn = paste0("var", law.fct),
            as.numeric(input$shape), as.numeric(input$rate)
        )
    })
    Etrunc <- shiny::reactive({
        shiny::req(less.than.d())
        rlang::exec(
            .fn = paste0("expValTrunc", law.fct),
            d = as.numeric(d()),
            as.numeric(input$shape), as.numeric(input$rate),
            less.than.d = ifelse(less.than.d(), TRUE, FALSE) ### here
        )
    })
    SL <- shiny::reactive({
        rlang::exec(
            .fn = paste0("stopLoss", law.fct),
            d = as.numeric(d()),
            as.numeric(input$shape), as.numeric(input$rate)
        )
    })
    Elim <- shiny::reactive({
        rlang::exec(
            .fn = paste0("expValLim", law.fct),
            d = as.numeric(d()),
            as.numeric(input$shape), as.numeric(input$rate)
        )
    })
    Mexcess <- shiny::reactive({
        rlang::exec(
            .fn = paste0("meanExcess", law.fct),
            d = as.numeric(d()),
            as.numeric(input$shape), as.numeric(input$rate)
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

    ####    Render translation  ####
    output$momentsTitle <- shiny::renderText({
        lang()$t("Moments")
    })
}

#' Interactive moments visualization (UI side)
#'
#' @param id id of module
#'
#' @return UI function for the moments module.
#'  Should not be run directly.
#'
#' @importFrom shiny tags NS uiOutput
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom tippy tippyOutput
#' @importFrom plotly plotlyOutput
#' @export
#'
#' @keywords internal
#'
momentsBoxUI <- function(id) {
    ns <- shiny::NS(id)

    shinydashboard::box(
        title = shiny::textOutput(ns("momentsTitle")),
        width = NULL,
        solidHeader = TRUE,
        status = "warning",
        shiny::uiOutput(ns("E")),
        shiny::withMathJax(tippy::tippyOutput(ns("E_tip"))),
        shiny::uiOutput(ns("V")),
        shiny::withMathJax(tippy::tippyOutput(ns("V_tip"))),
        shiny::uiOutput(ns("d")),
        shiny::splitLayout(
            shiny::withMathJax(shiny::uiOutput(ns("less.than.d"))),
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
}
