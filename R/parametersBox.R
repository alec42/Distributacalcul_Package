#' Interactive parameter chooser (server side)
#'
#' @param input input for server side.
#' @param output output for server side.
#' @param session session for server side.
#' @param law Distribution to visualize, one of ...
#' @param lang Internal function to ensure translation works and input is communicated between modules.
#'
#' @return Server function for the parameter module.
#'  Should not be run directly.
#'
#' @importFrom dplyr case_when
#' @importFrom ggplot2 ggplot stat_function aes labs theme_classic
#' @importFrom shiny reactive renderUI numericInput withMathJax
#' @export
#'
#' @keywords internal
#'
parametersBox <- function(input, output, session, law, lang) {
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

    #### Creates parameters (shape, rate) ####
    shape <- shiny::reactive({
        return(input$shape)
    })
    rate <- shiny::reactive({
        return(input$rate)
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
    ####    Render translation  ####
    output$parametersTitle <- shiny::renderText({
        lang()$t("Parameters")
    })
}

#' Interactive parameter chooser (UI side)
#'
#' @param id id of module
#'
#' @return UI function for the parameter module.
#'  Should not be run directly.
#'
#' @importFrom shiny NS uiOutput
#' @importFrom shinydashboardPlus boxPlus
#' @export
#'
#' @keywords internal
#'
parametersBoxUI <- function(id) {
    ns <- shiny::NS(id)
    #### Parameters ####
    shinydashboardPlus::boxPlus(
        title = shiny::textOutput(ns("parametersTitle")),
        status = "primary",
        background = "blue",
        solidHeader = TRUE,
        width = NULL,
        closable = FALSE,
        shiny::splitLayout(
            shiny::uiOutput(ns("shape")),
            shiny::uiOutput(ns("rate"))
        )
    )
}
