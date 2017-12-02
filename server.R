library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(zipcode)
library(data.table)

data(zipcode)

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 10, lat = 51, zoom = 6)
  })
  
  
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(auto.byZip[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(auto.byZip,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, auto.byZip$priceMean, breaks = 20)$breaks
  
  output$histPrice <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(zipsInBounds()$priceMean,
         breaks = centileBreaks,
         main = "Price",
         xlab = "Price",
         xlim = range(auto.byZip$priceMean),
         col = '#00DD00',
         border = 'white')
  })
  
  output$scatterPriceAge <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(priceMean ~ ageMean, data = zipsInBounds(), xlim = range(auto.byZip$ageMean), ylim = range(auto.byZip$priceMean)))
  })
  
  output$scatterPriceKilometer <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(priceMean ~ kilometerMean, data = zipsInBounds(), xlim = range(auto.byZip$kilometerMean), ylim = range(auto.byZip$priceMean)))
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the cer has chosen to map to color and size.
  observe({
    vehicleTypeBy <- input$vehicleType
    colorBy <- input$color
    sizeBy <- input$size
    
    cat(file=stderr(), "vehicleTypeBy:", vehicleTypeBy, ",colorBy:", colorBy, ",sizeBy:", sizeBy, "\n")

    if (vehicleTypeBy == "all") {
      auto.byZip_byVehicleType <- auto.byZip
    } else {
      auto.byZip_byVehicleType <- auto.byZip[auto.byZip$vehicleType == vehicleTypeBy,]
    }
    
    if (colorBy == "vehicleType") {
      colorData <- auto.byZip_byVehicleType[[colorBy]]
      pal <- colorFactor("Spectral", colorData)
    } else {
      colorData <- auto.byZip_byVehicleType[[colorBy]]
#      pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
      pal <- colorNumeric("Spectral", colorData)
    }
    
    radius <- auto.byZip_byVehicleType[[sizeBy]] / max(quantile(auto.byZip_byVehicleType[[sizeBy]], 0.90)) * 10000

    leafletProxy("map", data = auto.byZip_byVehicleType) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~postalCode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showCarPopup <- function(postalCode, lat, lng) {
    
    cat(file=stderr(), "vehicleTypeBy:", input$vehicleType, "\n")
    
    if (input$vehicleType == "all") {
      selectedZip <- auto.byZip[auto.byZip$latitude == lat & auto.byZip$longitude == lng,]
    } else {
      selectedZip <- auto.byZip[auto.byZip$latitude == lat & auto.byZip$longitude == lng & auto.byZip$vehicleType == input$vehicleType,]
    }

    content <- as.character(tagList(
      # tags$h4("Postal Code:", selectedZip$postalCode),
      tags$h4(selectedZip$postalCode, 
              subset(zipcode, zip == selectedZip$postalCode)[["city"]],
              subset(zipcode, zip == selectedZip$postalCode)[["state"]]
      ),
      # tags$strong(HTML(sprintf("(%s %s)",
      #                          selectedZip$latitude, selectedZip$longitude
      # ))), tags$br(), tags$br(),
      sprintf("Vehicle Type: %s", input$vehicleType), tags$br(),
      sprintf("Price Mean: %s", selectedZip$priceMean), tags$br(),
      sprintf("Price Median: %s", selectedZip$priceMedian), tags$br(),
      sprintf("Age Mean: %s", selectedZip$ageMean), tags$br(),
      sprintf("Age Median: %s", selectedZip$ageMedian), tags$br(),
      sprintf("Kilometer Mean: %s", selectedZip$kilometerMean), tags$br(),
      sprintf("Kilometer Median: %s", selectedZip$kilometerMedian), tags$br(),
      sprintf("Count: %s", selectedZip$count)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = postalCode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showCarPopup(event$id, event$lat, event$lng)
    })
  })
}
