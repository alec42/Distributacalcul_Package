#' Interactive parameter chooser (server side)
#'
#' @template input-template
#' @template output-template
#' @template session-template
#' @param law Distribution to visualize, one of ...
#' @template lang-template
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
                law.fct %in% c("Pareto", "Llogis") ~ 3,
                TRUE ~ 2
            ),
            min = dplyr::case_when(
                law.fct %in% c("Lnorm", "Gamma", "Exp", "Beta") ~ 0
            )
        )
    })
    output$rate <- shiny::renderUI({
        shiny::numericInput(
            inputId = session$ns("rate"),
            label = shiny::withMathJax(parameters_latex[2]),
            value = 3,
            min = dplyr::case_when(
                law.fct %in% c("Lnorm", "Norm", "Gamma", "Beta") ~ 0
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
#' @importFrom shiny NS uiOutput textOutput
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
