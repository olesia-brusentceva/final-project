ChooseCountryMapUI <- function(id)
{
  ns <- NS(id)
  ui <- fluidPage(
    tags$head(tags$style(
      HTML(
        "
    .leaflet-container {
    background-color:rgba(255,0,0,0.0);}
    "
        )
    )),
    theme = bs_theme(version = 4, bootswatch = "minty"),
    fluidRow(column(
      width = 12,
      selectizeInput(
        inputId = ns('searchCountry'),
        label = strong('Select Country'),
        choices = WDI_Countries$Country.Name,
        selected = c("Ukraine", "United States"),
        multiple = TRUE,
        # allow for multiple inputs
        options = list(create = FALSE),
        width = "100%",
      )
    ), ),
    fluidRow(
      helpText(
        "Start typing country or region name or select it on the map. You can select any number of countries. To clear your selection use Backspace to delete the last selected country, chose the country from the list and delete it, or clear all selection "
      )
    ),
    fluidRow(column(
      width = 12,
      withSpinner(leafletOutput(
        outputId = ns("myMap"), width = "100%"
      ), color = "#80c4ac")
    )),
    fluidRow(column(
      width = 12,
      actionButton(
        inputId = ns("clearHighlight")
        ,
        icon = icon(name = "eraser")
        ,
        label = "Clear Selection"
      )
    )),
    hr()
  )
}

ChooseCountryMapServer <- function(id)
{
  moduleServer(id, function(input, output, session)
  {
    click.list <- reactiveValues(ids = vector())
    groups <- reactiveValues(count = 0)
    
    foundational.map <- reactive({
      leaflet() %>%
        addPolygons(
          data = WB_CountryPolygons,
          layerId = WB_CountryPolygons$NAME_EN,
          group = "click.list",
          fill = TRUE,
          fillColor = "#f8f4f4",
          fillOpacity = 1,
          weight = 1,
          color = "#5a5a5a",
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#80c4ac",
            bringToFront = F
          ),
          label = as.character(WB_CountryPolygons$NAME_EN),
          labelOptions = labelOptions(noHide = FALSE)
        ) %>% setView(lng = 0,
                      lat = 20,
                      zoom = 1)
      
    })
    
    
    # observe where the user clicks on the leaflet map during the Shiny app session
    # Courtesy of two articles:
    # https://stackoverflow.com/questions/45953741/select-and-deselect-polylines-in-shiny-leaflet
    # https://rstudio.github.io/leaflet/shiny.html
    observeEvent(input$myMap_shape_click,
                 {
                   # store the click(s) over time
                   click <- input$myMap_shape_click
                   
                   # store the polygon ids which are being clicked
                   click.list$ids <- c(click.list$ids, click$id)
                   
                   # filter the spatial data frame by only including polygons which are stored in the click.list$ids object
                   lines.of.interest <-
                     WB_CountryPolygons[which(WB_CountryPolygons$NAME_EN %in% click.list$ids) , ]
                   
                   if (is.null(click$id))
                   {
                     req(click$id)
                   }
                   else if (!click$id %in% lines.of.interest@data$id)
                   {
                     # call the input proxy
                     updateSelectizeInput(session,
                                          "searchCountry",
                                          selected = c(input$searchCountry, click$id))
                   }
                 })
    
    observeEvent(input$searchCountry,
                 {
                   groups$count = groups$count + 1
                   
                   lines <-
                     WB_CountryPolygons[which(WB_CountryPolygons$NAME_EN %in% input$searchCountry), ]
                   
                   leafletProxy(mapId = "myMap") %>%
                     addPolygons(
                       data = lines,
                       layerId = lines@data$id,
                       group = as.character(groups$count),
                       fill = TRUE,
                       fillColor = "#f8949c",
                       fillOpacity = 1,
                       weight = 4,
                       color = "#80c4ac"
                     ) %>% clearGroup(as.character(groups$count - 1))
                 }, ignoreNULL = FALSE)
    
    observeEvent(input$clearHighlight,
                 {
                   updateSelectizeInput(session,
                                        "searchCountry",
                                        selected = "")
                   leafletProxy(mapId = "myMap") %>% clearGroup(as.character(groups$count))
                 })
    
    countries <- reactive({
      WDI_Countries[WDI_Countries$Country.Name %in% input$searchCountry, 2]
    })
    output$myMap <- renderLeaflet({
      foundational.map()
    })
    return(countries)
  })
}
