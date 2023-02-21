ChoosePriceUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    fluidRow(
      sliderInput(
        ns("price"),
        label = strong("Select price range"),
        min = 4000,
        max  = 500000,
        value = c(4000, 8614231),
        sep = "",
        width = "100%"
      )
    ),
    fluidRow(
      helpText(
        "Select price range to be displayed."
      )
    ),
    hr()
  )
}

ChoosePriceServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 output$price = reactive({input$price})
               })
}
