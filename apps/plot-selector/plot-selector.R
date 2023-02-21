source("apps/data/get-data.r")
source("apps/choose-country-list/choose-country-list.R")
source("apps/choose-indicator/choose-indicator.R")
source("apps/choose-date/choose-date.R")

PlotSelector <- function(id) {
  ns <- NS(id)
  fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    fluidRow(
      selectizeInput(
        inputId = "change_plot",
        label = "Visualize:",
        choices = c(
          `<i class='fa fa-line-chart'></i>` = "line",
          `<i class='fa fa-pie-chart'></i>` = "point"
        ),
        selected = "line"
      )
    ),
    fluidRow(
      helpText(
        "Choose what kind of plot would tou like to see"
      )
    ),
    hr()
  )
}

PlotSelectorServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
        
                 output$plot <- renderPlot({
                   
                   plotdata<-melt(setDT(selectData()),
                                  id.vars = c("country","iso2c","iso3c","year"),
                                  variable.name = "indicator")
                   #plotdata <- plotdata[dim(plotdata)[1]:1,]
                   
                   if(input$change_plot %in% "line"){
                     plot <- ggplot()+
                       geom_line(plotdata,
                                 mapping = aes(year, value, colour = country)) +
                       ylab("value") +
                       theme_classic() 
                     #+scale_color_gradientns()
                     return(plot)}
                   
                   else {
                     plot <- ggplot()+
                       geom_point(plotdata,
                                  mapping = aes(year, value, colour = country)) +
                       ylab("value") +
                       theme_classic() 
                     return(plot)
                   }
                 }, res = 96)
                 
               })
}

