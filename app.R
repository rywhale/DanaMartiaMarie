# Imports
library(shiny)
library(rtdd)
library(dplyr)
library(lubridate)
library(leaflet)
library(leaflet.esri)
library(sf)
library(ggplot2)
library(tidyr)
library(stringr)
library(glue)
library(tidyhydat)
library(purrr)
library(snakecase)

source("dana_utils.R")

ui <- fluidPage(
  theme = "danamartia.css",
  titlePanel("DanaMartia"),
  leafletOutput("stn_map", height = "800px"),
  hr(),
  sidebarLayout(
   
    sidebarPanel(
      htmlOutput("stn_text", class = "stn-text"),
      htmlOutput("stn_start", class = "stn-text"),
      htmlOutput("thresh_title", class = "stn-text"),
      tableOutput("thresh_table"),
      htmlOutput("link_text", class = "stn-text")
    ),
    mainPanel(
      plotOutput("stn_plot", height = "800px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  load("danamartia_cache.rda")
  
  # URL for LIO topographic map tiles
  lio_topo_url <- "https://ws.giscache.lrc.gov.on.ca/arcgis/rest/services/LIO_Cartographic/LIO_Topographic/MapServer"

  withProgress(
    message = "Gathering data", value = 0, {
      # Gauge metadata (including coords)
      gauge_meta <- rtdd::dd_hydro_meta(prov_terr = "ON")

      gauge_meta <- gauge_meta %>%
        st_as_sf(
          coords = c(
            "STATION_LON", "STATION_LAT"
          )
        )

      incProgress(1 / 4, detail = "Grabbing real-time station data")

      # All station data
      gauge_data <- rtdd::dd_hydro_data(
        prov_terr = "ON", all_stns = TRUE
      )
      
      if(!tidyhydat::hy_version()$Date <= hy_vers_date){
        
        incProgress(1 / 4, detail = "Updating historical information, this could take several minutes.")
        
        update_thresh(unique(gauge_data$STATION_ID))
        
        load("danamartia_cache.rda")
        
      }else{
        thresh_dat <- hy_thresh
      }

      # # Test with single station
      # gauge_data <- dd_hydro_data(station_id = "02HA006", prov_terr = "ON")

      # Combine parameter columns
      gauge_data <- gauge_data %>%
        gather(
          "Parameter", "Value",
          -c(
            "STATION_ID",
            "TIMESTAMP",
            "QA_QC_WL",
            "QA_QC_DIS"
          )
        ) %>%
        # Clean up parameter names
        mutate(
          Parameter = str_replace(
            Parameter,
            "WATER_LEVEL",
            "Water Level (m)"
          ) %>%
            str_replace(
              "DISCHARGE",
              "Discharge (cms)"
            )
        )

      # Filter stations to map to only include
      # those with values in the data file
      gauge_meta <- gauge_meta %>%
        filter(
          STATION_ID %in% gauge_data$STATION_ID
        )
      
      incProgress(2 / 4, detail = "Gathering historical stats for stations")

      gauge_data <- gauge_data %>%
        left_join(
          thresh_dat,
          by = c(
            "STATION_ID" = "station_number",
            "Parameter" = "Parameter"
          )
        )
      
      incProgress(3 / 4, detail = "Creating map")

      output$stn_map <- renderLeaflet({
        leaflet(gauge_meta) %>%
          addEsriTiledMapLayer(lio_topo_url) %>%
          addCircleMarkers(
            layerId = ~STATION_ID,
            clusterOptions = markerClusterOptions()
          )
      })

      incProgress(4 / 4, "Donezo")
    }
  )

  observeEvent(input$stn_map_marker_click, {
    stn_data <- gauge_data %>%
      filter(
        STATION_ID == input$stn_map_marker_click$id
      )
    
    output$thresh_table <- renderTable({
      thresh_dat %>%
        filter(
          station_number  == input$stn_map_marker_click$id
        )
    }, width = "100%")

    
    # stn_data %>%
    #   group_by(Parameter) %>%
    #   summarise(
    #     "Minimum for period" = min(Value, na.rm = TRUE),
    #     "Maximum for period" = max(Value, na.rm = TRUE)
    #   )
    
    output$thresh_title <- renderUI({
      HTML("<b> Percentiles for Period of Record </b><br>")
    })
    
    output$stn_text <- renderUI({
      HTML(glue(
        "<b>Station Name: </b><text>{snakecase::to_upper_camel_case(gauge_meta$STATION_NAME[gauge_meta$STATION_ID == input$stn_map_marker_click$id], sep_out = ' ')} <br></text>
        <b>Station Number: </b><text>{input$stn_map_marker_click$id}</text>"
      ))
    })

    # <b>Location: </b><text>{input$stn_map_marker_click$lat}, {input$stn_map_marker_click$lng}<br></text>"

    
    output$link_text <- renderUI({
      HTML(glue(
        "<text><a href='https://wateroffice.ec.gc.ca/report/real_time_e.html?stn={input$stn_map_marker_click$id}' 
        style = 'color: blue' target='_blank'>View on WSC Website</a>"
      ))
    })
    
    output$stn_start <- renderUI({
      HTML(glue(
        "<b> Active Since: </b><text>{get_min_stn_date(input$stn_map_marker_click$id)}<br></text>"
      ))
    })

    output$stn_plot <- renderPlot({
      plot_stn_data(stn_data)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
