ChooseRoomListUI <- function(id, all.choices) {
  ns <- NS(id)
  fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    fluidRow( 
      selectizeInput(
        inputId = ns('room'),
        label = strong('Select number of rooms'),
        choices = all.choices,
        selected = c("1","2"),
        multiple = TRUE,
        options = list(create = FALSE),
        width = "100%"
      )
    ),
    fluidRow(
      helpText(
        "Start typing or select from list"
      )
    ),
    hr()
  )
}

ChooseRoomListServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                  output$rooms <- reactive({input$room})
                 })
               }

