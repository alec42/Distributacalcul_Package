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
#' @importFrom rlang ".data" exec ns_env
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
            value = 2,
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
            value = 1,
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
                .fn = ifelse(law.fct == "pareto", paste0("d_", law.fct), paste0("d", law.fct)),
                x = as.numeric(x()),
                as.numeric(shape()), as.numeric(rate()),
                .env = rlang::ns_env(x = ifelse(law.fct == "pareto", 'Distributacalcul', 'stats'))
            ),
            nsmall = 6
        )
    })

    repartSurvie_distr <- shiny::reactive({
        format(
            rlang::exec(
                .fn = ifelse(law.fct == "pareto", paste0("p_", law.fct), paste0("p", law.fct)),
                q = as.numeric(x()),
                as.numeric(shape()), as.numeric(rate()),
                lower.tail = xlim_distr(),
                .env = rlang::ns_env(x = ifelse(law.fct == "pareto", 'Distributacalcul', 'stats'))
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
    Etronq <- shiny::reactive({
        shiny::req(less.than.d())
        rlang::exec(
            .fn = paste0("Etronq_", law.fct),
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
    output$Etronq <- shiny::renderUI({
        shiny::withMathJax(
            sprintf(
                "$$\\text{E}[X \\times 1_{\\{X %s %s\\}}] = %.4f$$",
                ifelse(less.than.d(), "\\geq", "\\leq"),
                as.numeric(d()),
                Etronq()
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
                        .fn = ifelse(law.fct == "pareto", paste0("VaR_", law.fct), paste0("q", law.fct)),
                        p = xx,
                        as.numeric(shape()), as.numeric(rate()),
                        .env = rlang::ns_env(x = ifelse(law.fct == "pareto", 'Distributacalcul', 'stats'))
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
                        .fn = paste0(ifelse(plot_choice_QX() == "Density Function", ifelse(law == "pareto", "d_", "d"), ifelse(law == "pareto", "p_", "p")), law),
                        xx,
                        as.numeric(shape()), as.numeric(rate()),
                        .env = rlang::ns_env(x = ifelse(law.fct == "pareto", 'Distributacalcul', 'stats'))
                    )),
                    alpha = 0.7
                ) +
                ggplot2::stat_function(
                    fun = Vectorize(function(xx) rlang::exec(
                        .fn = paste0(ifelse(plot_choice_QX() == "Density Function",
                                            ifelse(law == "pareto", "d_", "d"),
                                            ifelse(law == "pareto", "p_", "p"))
                                     ,law
                        ),
                        xx,
                        as.numeric(shape()), as.numeric(rate()),
                        .env = rlang::ns_env(x = ifelse(law.fct == "pareto", 'Distributacalcul', 'stats'))
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
                    .fn = paste0(ifelse(plot_choice_FSX() == "Density Function", ifelse(law.fct == "pareto", "d_", "d"), ifelse(law.fct == "pareto", "p_", "p")), law),
                    xx,
                    as.numeric(shape()), as.numeric(rate()),
                    .env = rlang::ns_env(x = ifelse(law.fct == "pareto", 'Distributacalcul', 'stats'))
                )),
                alpha = 0.7
            ) +
            ggplot2::stat_function(
                fun = Vectorize(function(xx) rlang::exec(
                    .fn = paste0(ifelse(plot_choice_FSX() == "Density Function", ifelse(law.fct == "pareto", "d_", "d"), ifelse(law.fct == "pareto", "p_", "p")), law),
                    xx,
                    as.numeric(shape()), as.numeric(rate()),
                    .env = rlang::ns_env(x = ifelse(law.fct == "pareto", 'Distributacalcul', 'stats'))
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
                tippy::tippyOutput(ns("E_tip")),
                shiny::uiOutput(ns("V")),
                tippy::tippyOutput(ns("V_tip")),
                shiny::uiOutput(ns("d")),
                shiny::splitLayout(
                    shiny::uiOutput(ns("less.than.d")),
                    shiny::uiOutput(ns("Etronq")),
                    cellWidths = 'auto'
                ),
                shiny::uiOutput(ns("Elim")),
                shiny::uiOutput(ns("SL")),
                shiny::uiOutput(ns("Mexcess"))
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
                shiny::uiOutput(ns("TVaR")),
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
                shiny::uiOutput(ns("xlim_distr")),
                shiny::uiOutput(ns("repartSurvie_distr")),
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
