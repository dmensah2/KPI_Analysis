#read in packages
library(sp);library(sf);library(ggplot2)
library(classInt);library(leaflet);library(usethis)
library(ggpubr);library(devtools);library(spatialEco)
library(shinyWidgets);library(shinythemes);library(profvis)
library(ggpubr);library(shiny);library(scales)

#----------DATA PROCESSING-----------#
# List of months
choices_month <- format(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2020-10-01"), by = "month"), "%B-%Y")

#read in listings shapefile
listings_by_county <- sf::st_read("listings_by_county.shp") %>%
  #transform crs of shapefile
  sf::st_transform(4326)

#convert to sp object so it can work with everything (sp is more recent than sf)
listings_by_county <- sf::as_Spatial(listings_by_county, cast = TRUE)

#----------USER INTERFACE-----------#
ui <- fluidPage(
       theme = shinytheme("flatly"),
        titlePanel("Weedmaps Revenue"),
        leafletOutput("map", width = "100%", height = "900px"),
              absolutePanel(top = 70, right = 20,
                      sliderTextInput(inputId = "date",
                                  label = "Choose month:",
                                  #vector of choices
                                  choices = choices_month,
                                  #default
                                  selected = "October-2020",
                                  force_edges = TRUE),

                      selectInput(inputId = "channel",
                                  label = "Choose Retail Type: ",
                                  choices = c("All", "Delivery","Dispensary"),
                                  selected = "All"),

                      checkboxInput("legend", "Show legend", TRUE)
         )
      )

#--------SERVER PREPARATION-----#
server <- function(input, output, session) {

  #prepare output by using renderLeaflet function for static map components
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng =-98.579480, lat=39.828358, zoom = 3.5) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })

  #create reactive data from filter
  filteredData <- reactive ({
    listings_by_county[listings_by_county$Mnth_Yr == input$date[1] & listings_by_county$ctgr2__ == input$channel[1],]
  })

  #observe and leafletproxy for dynamic map components; observer to create chloropleth component of map
  observe({
    #create map highlight
    map_highlight <- highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.5,
      bringToFront = TRUE)

    #create popup that depends on filtered data (these are independent)
    map_popup <- ~paste("<strong>County: </strong>",
                        County,
                        "<br/>",
                        "<strong>Month: </strong>",
                        Mnth_Yr,
                        "<br/>",
                        "<strong>Listing Count: </strong>",
                        lstng_c,
                        "<br/>",
                        "<strong>Total Revenue: </strong>",
                        ttl_rv_,
                        "<br/>",
                        "<strong>Retail Type: </strong>",
                        ctgr2__,
                        sep="")

    #create leaflet proxy to pass in reactive data
    leafletProxy("map", data = filteredData()) %>%
      #clear features when interacted with
      clearShapes() %>%
      #add polygons
      addPolygons(weight = 1,
                  smoothFactor = .5,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  #needs to match pal in legend
                  fillColor = ~colorQuantile("YlGnBu", ttl_rvn, n = 10)(ttl_rvn),
                  highlight = map_highlight,
                  popup = map_popup)
  })

  #separate observer to recreate the legend as needed
  observe ({
    #create leaflet proxy to pass in reactive data and store it as variable
    proxy <- leafletProxy("map", data = listings_by_county)
    #if the input is TRUE create legend
    if (input$legend) {
      proxy %>%
        addLegend(position = "bottomleft",
                  title = "Total Revenue",
                  #needs to match fillColor(addPolygons) or color(addMarkers)
                  pal = colorQuantile("YlGnBu", listings_by_county$ttl_rvn, n = 10),
                  opacity = 1,
                  values = ~ttl_rvn)
    }
  })
}

shinyApp(ui = ui, server = server)