library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

source("apps/data/get-data.r")
source("apps/choose-country-list/choose-country-list.R")
source("apps/choose-indicator/choose-indicator.R")
source("apps/choose-date/choose-date.R")

# APP
ui <- dashboardPage(
  dashboardHeader(title = "ShinyWidget Plot Change"),
  dashboardSidebar(
    ChooseCountryListUI("1"),
    ChooseIndicatorListUI("2"),
    ChooseDateUI("3")
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", brush = "plot_brush", height = 250),
          tableOutput("data")),
      div(radioGroupButtons(
        inputId = "change_plot",
        label = "Visualize:",
        choices = c(
          `<i class='fa fa-line-chart'></i>` = "line",
          `<i class='fa fa-circle fa-2xs'></i>` = "scatter"
        ),
        justified = TRUE,
        selected = "line"
      ))
    )
  )
)

server = function(input, output) {
  
  country.list <- ChooseCountryMapServer("1")
  indicator.list <- ChooseIndicatorListServer("2")
  date.list <- ChooseDateServer("3")
  
  selectData <- eventReactive(input$update,{
    melt(setDT(WDI(
      country = country.list(),
      indicator = indicator.list(),
      start = date.list()[1],
      end = date.list()[2]
    )),id.vars = c("country","iso2c","iso3c","year"),
    variable.name = "indicator")
  },ignoreNULL = FALSE)
  
  output$plot <- renderPlot({
    
    plotdata<-selectData()
    splotdata <- plotdata[dim(plotdata)[1]:1,]
    
    if (input$change_plot %in% "line") {
      plot <- ggplot() +
        geom_line(splotdata, mapping = aes(year, value, colour = country)) +
        ylab("value") +
        theme_classic() +
        scale_fill_brewer(palette = "RdPu")
      return(plot)
    } else {
      plot <- ggplot() +
        geom_area(splotdata, mapping = aes(year, value, fill = country)) +
        ylab("value") +
        theme_classic() +
        scale_fill_brewer(palette = "YIGn")
      return(plot)
    }
    
  }, res = 96)
  output$data <- renderTable({
    
    plotdata<-selectData()
    
    brushedPoints(plotdata, input$plot_brush)
  })
  
}
shinyApp(ui, server)


x <- c(3:5, 11:8, 8 + 0:5)
(ux <- length(unique(x)))
(u2 <- unique(x, fromLast = TRUE)) # different order
stopifnot(identical(sort(ux), sort(u2)))

length(unique(sample(100, 100, replace = TRUE)))
## approximately 100(1 - 1/e) = 63.21

unique(iris)