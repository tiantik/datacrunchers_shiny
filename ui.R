library(leaflet)

vars_color <- c(
  "Price Mean" = "priceMean",
  "Price Median" = "priceMedian",
  "Kilometer Mean" = "kilometerMean",
  "Kilometer Median" = "kilometerMedian",
  "Age Mean " = "ageMean",
  "Age Median" = "ageMedian",
  "Vehicle Type" = "vehicleType"
)

vars_size <- c(
  "Price Mean" = "priceMean",
  "Price Median" = "priceMedian",
  "Kilometer Mean" = "kilometerMean",
  "Kilometer Median" = "kilometerMedian",
  "Age Mean " = "ageMean",
  "Age Median" = "ageMedian",
  "Count" = "count"
)

navbarPage("Data Crunchers - Used Cars in Germany", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Data Filters"),

        selectInput("vehicleType", "Vehicle Type", factor(c(as.character(unique(auto$vehicleType)), "all"))),
        selectInput("color", "Bubble Color", vars_color),
        selectInput("size", "Bubble Size", vars_size),

        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),
        
        plotOutput("histPrice", height = 200),
        plotOutput("scatterPriceAge", height = 250),
        plotOutput("scatterPriceKilometer", height = 250)
      )
    )
  )
)
