#' Interactive functions visualization (server side)
#'
#' @param input input for server side.
#' @param output output for server side.
#' @param session session for server side.
#' @param law Distribution to visualize, one of ...
#' @param lang Internal function to ensure translation works and input is communicated between modules.
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

    #### Render LaTeX ####
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
                law.fct %in% c("lnorm", "gamma", "exp", "beta") ~ 0,
                law.fct == c("unif") ~ as.numeric(input$shape)
            ),
            max = dplyr::case_when(
                law.fct == "beta" ~ 1,
                law.fct == "unif" ~ as.numeric(input$rate)
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
                .fn = paste0("d", law.fct),
                x = as.numeric(x()),
                as.numeric(input$shape), as.numeric(input$rate),
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
                as.numeric(input$shape), as.numeric(input$rate),
                lower.tail = xlim_distr(),
                .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
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
                .fn = paste0("VaR_", law),
                kap = 0.01,
                as.numeric(input$shape), as.numeric(input$rate)
            ), rlang::exec(
                .fn = paste0("VaR_", law),
                kap = 0.99,
                as.numeric(input$shape), as.numeric(input$rate)
            ))), ggplot2::aes_(x = ~x.limz)) +
            ggplot2::stat_function(
                fun = Vectorize(function(xx) rlang::exec(
                    .fn = paste0(ifelse(plot_choice_FSX() == lang()$t("Density Function"), "d", "p"), law),
                    xx,
                    as.numeric(input$shape), as.numeric(input$rate),
                    .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                )),
                alpha = 0.7
            ) +
            ggplot2::stat_function(
                fun = Vectorize(function(xx) rlang::exec(
                    .fn = paste0(ifelse(plot_choice_FSX() == lang()$t("Density Function"), "d", "p"), law),
                    xx,
                    as.numeric(input$shape), as.numeric(input$rate),
                    .env = rlang::ns_env(x = ifelse(law.fct %in% c("pareto", "llogis"), 'Distributacalcul', 'stats'))
                )),
                xlim = c(
                    ifelse(xlim_distr(), rlang::exec(
                        .fn = paste0("VaR_", law),
                        kap = 0.01,
                        as.numeric(input$shape), as.numeric(input$rate)
                    ), x()),
                    ifelse(xlim_distr(),
                           x(),
                           rlang::exec(
                               .fn = paste0("VaR_", law),
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
