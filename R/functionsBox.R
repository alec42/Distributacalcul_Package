#' Interactive functions visualization (server side)
#'
#' @template input-template
#' @template output-template
#' @template session-template
#' @param law Distribution to visualize, one of ...
#' @template lang-template
#'
#' @return Server function for the functions module.
#'  Should not be run directly.
#'
#' @importFrom rlang exec
#' @importFrom ggplot2 ggplot stat_function aes labs theme_classic
#' @importFrom dplyr case_when
#' @importFrom tippy renderTippy tippy_this
#' @importFrom shiny req reactive renderUI numericInput withMathJax
#' @importFrom shinyWidgets switchInput pickerInput
#' @export
#'
#' @keywords internal
#'
functionsBox <- function(input, output, session, law, lang) {
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
        # law == "Burr" ~ c("$$\\alpha$$", "$$\\lambda$$", "$$\\tau$$"),
        TRUE ~ c("shape", "rate")
    )

    #### Render LaTeX ####
    output$density_distr_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("density_distr"),
            tooltip = paste0("$f(x) = $", dplyr::case_when(
                law == "Norm" ~ "$\\frac{{e}^{-\\frac{(x - \\mu)^{2}}{2 \\sigma^{2}}}}{\\sqrt{2\\pi}\\sigma}$",
                law == "Lnorm" ~ "$\\frac{1}{x \\sqrt{2\\pi} \\sigma} \\mathrm{e}^{-\\frac{(\\ln(x) - \\mu )^{2}}{2 \\sigma^{2}}}$",
                law == "Gamma" ~ "$\\frac{\\beta^{\\alpha}}{\\Gamma (\\alpha)} x^{\\alpha - 1} \\mathrm{e}^{-\\beta x}$",
                law == "Exp" ~ "$\\beta \\mathrm{e}^{-\\beta x}$",
                law == "Llogis" ~ "$\\frac{\\tau \\lambda^\\tau x^{\\tau - 1}}{(\\lambda^{\\tau} + x^{\\tau})^{2}}$",
                law == "Weibull" ~ "$\\beta \\tau \\left(\\beta x\\right)^{\\tau - 1} \\mathrm{e}^{-\\left(\\beta x\\right)^{\\tau}}$",
                law == "Beta" ~ "$\\frac{x^{\\alpha -1}\\left(1 - x\\right)^{\\beta - 1}}{I\\left(\\alpha, \\beta\\right)} \\times 1_{\\left\\{x \\in \\left[0, 1\\right]\\right\\}}$",
                law == "Unif" ~ "$\\frac{1}{b - a}\\times 1_{\\left\\{x \\in \\left[a, b\\right]\\right\\} }$",
                law == "Pareto" ~ "$\\frac{\\alpha \\lambda^{\\alpha}}{\\left(\\lambda + x\\right)^{\\alpha + 1}}$",
                TRUE ~ "f(x)"
            ))
        )
    })
    output$repartSurvie_distr_tip <- tippy::renderTippy({
        tippy::tippy_this(
            ns("repartSurvie_distr"),
            tooltip = paste0("$F_{X}(x) = $", dplyr::case_when(
                law == "Norm" ~ "$\\Phi\\left(\\frac{x - \\mu}{\\sigma}\\right)$",
                law == "Lnorm" ~ "$\\Phi\\left(\\frac{\\ln(x) - \\mu}{\\sigma}\\right)$",
                law == "Gamma" ~ "$H\\left(x; \\alpha, \\beta \\right)$",
                law == "Exp" ~ "$1 - \\mathrm{e}^{-\\beta x}$",
                law == "Llogis" ~ "$1 - \\frac{\\lambda^{\\tau}}{\\lambda^\\tau + x^\\tau}$",
                law == "Weibull" ~ "$1 - \\mathrm{e}^{-\\left(\\beta x\\right)^{\\tau}}$",
                law == "Beta" ~ "$B\\left(x; \\alpha, \\beta\\right)$",
                # law == "Unif" ~ "$\\left\\{\\begin{array}{ll}
                # 0, & x<a \\\\
                # \\frac{x-a}{b-a}, & a\\leq x\\leq b \\\\
                # 1, & x>b%
                # \\end{array}$",
                law == "Pareto" ~ "$1 - \\left(\\frac{\\lambda}{\\lambda + x}\\right)^{\\alpha}$",
                TRUE ~ "F_{X}(x)"
            ))
        )
    })


    #### Creates input (x, xlim_distr) ####
    x <- shiny::reactive({
        input$x
    })
    xlim_distr <- shiny::reactive({
        input$xlim_distr
    })

    #### Creates input for the plots (which function to plot) ####
    plot_choice_FSX <- shiny::reactive({
        input$plot_choice_FSX
    })

    #### Render input (x, xlim_distr) ####
    output$x <- shiny::renderUI({
        shiny::req(input$shape, input$rate)
        shiny::numericInput(
            inputId = session$ns("x"),
            label = shiny::withMathJax("$$x$$"),
            value = 2,
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
    output$xlim_distr <- shiny::renderUI({
        shinyWidgets::switchInput(
            inputId = session$ns("xlim_distr"),
            onStatus = "success",
            onLabel = lang()$t("Cumulative Density Function"),
            offStatus = "info",
            offLabel = lang()$t("Survival Function"),
            value = TRUE,
            labelWidth = "10px"
        )
    })

    #### Render input for the plots (which function to plot) ####
    output$plot_choice_FSX <- shiny::renderUI({
        shinyWidgets::pickerInput(
            inputId = session$ns("plot_choice_FSX"),
            choices = c(
                lang()$t("Density Function"),
                lang()$t("Cumulative Density Function")
            ),
            selected = lang()$t("Density Function"),
            options = list(
                style = "btn-success"
            )
        )
    })

    #### Calculate functions ####
    density_distr <- shiny::reactive({
        format(
            rlang::exec(
                .fn = paste0("d", ifelse(law.fct %in% c("Pareto", "Llogis"), law.fct, tolower(law.fct))),
                x = as.numeric(x()),
                as.numeric(input$shape), as.numeric(input$rate),
                .env = rlang::ns_env(x = ifelse(law.fct %in% c("Pareto", "Llogis"), 'Distributacalcul', 'stats'))
            ),
            nsmall = 6
        )
    })
    repartSurvie_distr <- shiny::reactive({
        format(
            rlang::exec(
                .fn = paste0("p", ifelse(law.fct %in% c("Pareto", "Llogis"), law.fct, tolower(law.fct))),
                q = as.numeric(x()),
                as.numeric(input$shape), as.numeric(input$rate),
                lower.tail = xlim_distr(),
                .env = rlang::ns_env(x = ifelse(law.fct %in% c("Pareto", "Llogis"), 'Distributacalcul', 'stats'))
            ),
            nsmall = 6,
            scientific = FALSE
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
    output$FSx <- plotly::renderPlotly({
        shiny::req(input$shape, input$rate, x(), plot_choice_FSX())
        ggplot2::ggplot(data = data.frame(
            x.limz = c(rlang::exec(
                .fn = paste0("VatR", law),
                kap = 0.01,
                as.numeric(input$shape), as.numeric(input$rate)
            ), rlang::exec(
                .fn = paste0("VatR", law),
                kap = 0.99,
                as.numeric(input$shape), as.numeric(input$rate)
            ))), ggplot2::aes_(x = ~x.limz)) +
            ggplot2::stat_function(
                fun = Vectorize(function(xx) rlang::exec(
                    .fn = paste0(ifelse(plot_choice_FSX() == lang()$t("Density Function"), "d", "p"), ifelse(law.fct %in% c("Pareto", "Llogis"), law.fct, tolower(law.fct))),
                    xx,
                    as.numeric(input$shape), as.numeric(input$rate),
                    .env = rlang::ns_env(x = ifelse(law.fct %in% c("Pareto", "Llogis"), 'Distributacalcul', 'stats'))
                )),
                alpha = 0.7
            ) +
            ggplot2::stat_function(
                fun = Vectorize(function(xx) rlang::exec(
                    .fn = paste0(ifelse(plot_choice_FSX() == lang()$t("Density Function"), "d", "p"), ifelse(law.fct %in% c("Pareto", "Llogis"), law.fct, tolower(law.fct))),
                    xx,
                    as.numeric(input$shape), as.numeric(input$rate),
                    .env = rlang::ns_env(x = ifelse(law.fct %in% c("Pareto", "Llogis"), 'Distributacalcul', 'stats'))
                )),
                xlim = c(
                    ifelse(xlim_distr(), rlang::exec(
                        .fn = paste0("VatR", law),
                        kap = 0.01,
                        as.numeric(input$shape), as.numeric(input$rate)
                    ), x()),
                    ifelse(xlim_distr(),
                           x(),
                           rlang::exec(
                               .fn = paste0("VatR", law),
                               kap = 0.99,
                               as.numeric(input$shape), as.numeric(input$rate)
                           )
                    )
                ),
                geom = "area",
                fill = ifelse(xlim_distr(), "Dark Green", "Royal Blue"),
                alpha = 0.7
            ) +
            ggplot2::theme_classic() +
            ggplot2::labs(
                x = "$$x$$",
                y = dplyr::case_when(
                    plot_choice_FSX() == lang()$t("Cumulative Density Function") ~ "$$F_{X}(x)$$",
                    plot_choice_FSX() == lang()$t("Density Function") ~ "$$f_{X}(x)$$"
                )
            )
    })
    ####    Render translation  ####
    output$functionsTitle <- shiny::renderText({
        lang()$t("Functions")
    })
}

#' Interactive moments visualization (UI side)
#'
#' @param id id of module
#'
#' @return UI function for the moments module.
#'  Should not be run directly.
#'
#' @importFrom shiny NS uiOutput
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom tippy tippyOutput
#' @importFrom plotly plotlyOutput
#' @export
#'
#' @keywords internal
#'
functionsBoxUI <- function(id) {
    ns <- shiny::NS(id)

    shinydashboardPlus::boxPlus(
        title = shiny::textOutput(ns("functionsTitle")),
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
    )
}
