ChooseAreaUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    fluidRow(
      sliderInput(
        ns("arearange"),
        label = strong("Select area range"),
        min = 3,
        max  = 1279,
        value = c(40, 80),
        sep = "",
        width = "100%"
      )
    ),
    fluidRow(
      helpText(
        "Select area range to be displayed."
      )
    ),
    hr()
  )
}

ChooseAreaServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 output$area = reactive ({input$arearange})
               })
}


