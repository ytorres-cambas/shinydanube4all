library(shiny)
library(leaflet)
library(leaflet.extras)  # For drawing tools
library(dplyr)
library(plotly)
library(RColorBrewer)
library(bslib)
library(sf)
library(DT)  # For the scrollable table

# Sample dataset with lat/lon points
#load("./data/points_data.rda")
# Sample data with multiple records per species
points_data <- data.frame(
  occurrenceID = 1:10,
  collectionCode = c("COL001", "COL001", "COL002", "COL002", "COL003", "COL003", "COL004", "COL004", "COL004", "COL004"),
  coordinatePrecision = c(0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.04, 0.04),
  licence = rep("CC BY 4.0", 10),
  rightsHolder = c("Institution A", "Institution A", "Institution B", "Institution B", "Institution C", "Institution C", "Institution D", "Institution D", "Institution D", "Institution D"),
  scientificName = c("Fish A", "Fish A", "Fish B", "Fish B", "Fish C", "Fish C", "Fish D", "Fish D", "Fish D", "Fish D"),
  genus = c("Genus A", "Genus A", "Genus B", "Genus B", "Genus C", "Genus C", "Genus D", "Genus D", "Genus D", "Genus D"),
  family = c("Family A", "Family A", "Family B", "Family B", "Family C", "Family C", "Family D", "Family D", "Family D", "Family D"),
  order = c("Order A", "Order A", "Order B", "Order B", "Order C", "Order C", "Order D", "Order D", "Order D", "Order D"),
  verbatimIdentification = c("Verbatim A", "Verbatim A", "Verbatim B", "Verbatim B", "Verbatim C", "Verbatim C", "Verbatim D", "Verbatim D", "Verbatim D", "Verbatim D"),
  institutionCode = c("INST001", "INST001", "INST002", "INST002", "INST003", "INST003", "INST004", "INST004", "INST004", "INST004"),
  catalogNumber = c("CAT001", "CAT001", "CAT002", "CAT002", "CAT003", "CAT003", "CAT004", "CAT004", "CAT004", "CAT004"),
  datasetName = c("Dataset 1", "Dataset 1", "Dataset 2", "Dataset 2", "Dataset 3", "Dataset 3", "Dataset 4", "Dataset 4", "Dataset 4", "Dataset 4"),
  occurrenceRemarks = c("Remark A", "Remark A", "Remark B", "Remark B", "Remark C", "Remark C", "Remark D", "Remark D", "Remark D", "Remark D"),
  samplingProtocol = c("Protocol A", "Protocol A", "Protocol B", "Protocol B", "Protocol C", "Protocol C", "Protocol D", "Protocol D", "Protocol D", "Protocol D"),
  basisOfRecord = c("Observation", "Observation", "Observation", "Observation", "Specimen", "Specimen", "Observation", "Observation", "Observation", "Observation"),
  day = c(12, 12, 15, 15, 8, 8, 10, 10, 10, 10),
  month = c(5, 5, 6, 6, 7, 7, 8, 8, 8, 8),
  year = c(2021, 2021, 2019, 2020, 2021, 2020, 2022, 2022, 2022, 2022),
  recordedBy = c("Contact A", "Contact A", "Contact B", "Contact B", "Contact C", "Contact C", "Contact D", "Contact D", "Contact D", "Contact D"),
  identifiedBy = c("Identifier A", "Identifier A", "Identifier B", "Identifier B", "Identifier C", "Identifier C", "Identifier D", "Identifier D", "Identifier D", "Identifier D"),
  individualCount = c(10, 10, 15, 15, 5, 5, 8, 8, 8, 8),
  organismQuantity = c(100, 100, 150, 150, 50, 50, 80, 80, 80, 80),
  organismQuantityType = c("Weight", "Weight", "Weight", "Weight", "Weight", "Weight", "Weight", "Weight", "Weight", "Weight"),
  lifeStage = c("Adult", "Adult", "Juvenile", "Juvenile", "Adult", "Adult", "Juvenile", "Juvenile", "Juvenile", "Juvenile"),
  sex = c("Male", "Male", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "Male"),
  locality = c("Location 1", "Location 1", "Location 2", "Location 2", "Location 3", "Location 3", "Location 4", "Location 4", "Location 4", "Location 4"),
  waterBody = c("Danube", "Danube", "Danube", "Danube", "Danube", "Danube", "Danube", "Danube", "Danube", "Danube"),
  waterBodyType = c("River", "River", "River", "River", "River", "River", "River", "River", "River", "River"),
  decimalLatitude = c(48.21, 48.21, 47.07, 47.07, 44.43, 44.43, 45.26, 45.26, 45.26, 45.26),
  decimalLongitude = c(16.37, 16.37, 19.45, 19.45, 20.27, 20.27, 18.50, 18.50, 18.50, 18.50),
  geodeticDatum = rep("WGS84", 10),
  coordinateUncertaintyInMeters = c(10, 10, 20, 20, 30, 30, 40, 40, 40, 40),
  georeferencedBy = c("Georef A", "Georef A", "Georef B", "Georef B", "Georef C", "Georef C", "Georef D", "Georef D", "Georef D", "Georef D"),
  georeferenceProtocol = c("Protocol X", "Protocol X", "Protocol Y", "Protocol Y", "Protocol Z", "Protocol Z", "Protocol W", "Protocol W", "Protocol W", "Protocol W"),
  georeferenceSources = c("Source A", "Source A", "Source B", "Source B", "Source C", "Source C", "Source D", "Source D", "Source D", "Source D"),
  georeferenceRemarks = c("Remark 1", "Remark 1", "Remark 2", "Remark 2", "Remark 3", "Remark 3", "Remark 4", "Remark 4", "Remark 4", "Remark 4"),
  municipality = c("Municipality 1", "Municipality 1", "Municipality 2", "Municipality 2", "Municipality 3", "Municipality 3", "Municipality 4", "Municipality 4", "Municipality 4", "Municipality 4"),
  stateProvince = c("Province 1", "Province 1", "Province 2", "Province 2", "Province 3", "Province 3", "Province 4", "Province 4", "Province 4", "Province 4"),
  country = c("Austria", "Austria", "Hungary", "Hungary", "Serbia", "Serbia", "Romania", "Romania", "Romania", "Romania"),
  reference = c("Ref 1", "Ref 1", "Ref 2", "Ref 2", "Ref 3", "Ref 3", "Ref 4", "Ref 4", "Ref 4", "Ref 4"),
  bibliographicCitation = c("Citation 1", "Citation 1", "Citation 2", "Citation 2", "Citation 3", "Citation 3", "Citation 4", "Citation 4", "Citation 4", "Citation 4"),
  gbifID = c(12345, 12346, 23456, 23457, 34567, 34568, 45678, 45679, 45680, 45681),
  subcatchmentId = c("Subcatchment A", "Subcatchment A", "Subcatchment B", "Subcatchment B", "Subcatchment C", "Subcatchment C", "Subcatchment D", "Subcatchment D", "Subcatchment D", "Subcatchment D")
)


# Load the polygon data (Danube basin)
load("./data/danube_basin.rda")

# Convert the data to sf object for spatial operations
points_sf <- st_as_sf(points_data,
                      coords = c("decimalLongitude", "decimalLatitude"),
                      crs = 4326)

# Content for the sidebar 
side_bar_content <-   accordion(  
  accordion_panel( 
    title = "Species", 
    icon = bsicons::bs_icon("menu-app"),
    selectInput( 
      "select_species", 
      "Select a species below:", 
      choices = unique(points_data$scientificName),
      selected = unique(points_data$scientificName),
      multiple = TRUE 
    )
  ),  
  accordion_panel(
    title = "Year",
    icon = bsicons::bs_icon("calendar"),
    sliderInput( 
      "slider_year",
      "Select a range", 
      min = min(points_data$year),
      max = max(points_data$year), 
      value =  c(min(points_data$year), max(points_data$year)),
      step = 1,
      sep = ""
    )
  ),  
  accordion_panel(
    title = "Locality",
    icon = bsicons::bs_icon("geo-alt"),
    selectInput( 
      "select_locality", 
      "Select a locality below:", 
      choices = unique(points_data$locality),
      selected = unique(points_data$locality), 
      multiple = TRUE 
    )
  ),  
  accordion_panel(
    title = "Country",
    icon = bsicons::bs_icon("globe-americas"), 
    checkboxGroupInput( 
      "checkbox_country", 
      "Select a country", 
      choices = unique(points_data$country),
      selected = unique(points_data$country)
    ) 
  ), 
  accordion_panel(
    title = "Contact person",
    icon = bsicons::bs_icon("person-lines-fill"), 
    checkboxGroupInput( 
      "checkbox_contact", 
      "Select a contact person", 
      choices = unique(points_data$rightsHolder),
      selected = unique(points_data$rightsHolder)
    )  
  ),
  accordion_panel(
    title = "Dataset",
    icon = bsicons::bs_icon("database"), 
    checkboxGroupInput( 
      "checkbox_dataset", 
      "Select a country", 
      choices = unique(points_data$datasetName),
      selected = unique(points_data$datasetName)
    )  
  ),
  id = "acc",  
  open = "Species"  
) 

# Content for METRICS
metrics_page <- page_fillable(" ", 
                              layout_columns(card("NUMBER OF RECORDS PER SPECIES",
                                                  plotlyOutput(outputId =
                                                                 "plot_species")),  
                                             card("NUMBER OF RECORDS PER YEAR",
                                                  plotlyOutput(outputId = 
                                                                 "plot_year")),
                                             card("NUMBER OF RECORDS PER COUNTRY",
                                                  plotlyOutput(outputId = 
                                                                 "plot_country")),
                                             card("NUMBER OF RECORDS PER DATASET",
                                                  plotlyOutput(outputId = 
                                                                 "plot_dataset"))
                                             )
                              ) 


ui <- page_sidebar(
  #theme = shinytheme("cosmo"),  # Use the Cosmo theme
  title = "Danube Fish Species Occurrences",
  sidebar = sidebar(
    side_bar_content,
    downloadButton("download_occurr", "Download")
  ),
  navset_tab(
    nav_panel("MAP", leafletOutput("map", height = 600)),
    nav_panel("TABLE",  DTOutput("filtered_points")),
    nav_panel("METRICS", metrics_page)
  ),
  
  footer <- tags$footer(
    style = "position: fixed; bottom: 0; width: 100%; text-align: center; padding: 10px; background-color: #f8f9fa; border-top: 1px solid #ddd;",
    "© 2024 ", 
    tags$a(href = "https://www.danube4allproject.eu/", target = "_blank", "DANUBE4all Project"), 
    " | All Rights Reserved"
  )
  
)


# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Reactive value to store the drawn shape coordinates
  drawn_shape <- reactiveVal(NULL)
  
  # Render the leaflet map with drawing tools for only polygon and rectangle
  output$map <- renderLeaflet({
    leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%  # Change tile provider
      # Add the Danube basin polygon 
      addPolygons(data = danube_basin, 
                  color = "blue", 
                  weight = 2,
                  fillColor = "lightblue",
                  fillOpacity = 0.5,
                  popup = "Danube Basin",
                  group = "Danube Basin") %>%
      # Add initial markers for all points
      addMarkers(data = points_data, 
                 ~decimalLongitude,
                 ~decimalLatitude, 
                 popup = ~paste0("<strong>Species:</strong> ", scientificName, 
                                 "<br><strong>Year:</strong> ", year, 
                                 "<br><strong>Location:</strong> ", locality,
                                 "<br><strong>Country:</strong> ", country,
                                 "<br><strong>Dataset:</strong> ", datasetName,
                                 "<br><strong>Contact Person:</strong> ", rightsHolder)) %>%
      # Add the draw toolbar with only polygon and rectangle options
      addDrawToolbar(
        targetGroup = "draw", 
        polygonOptions = drawPolygonOptions(),
        rectangleOptions = drawRectangleOptions(),
        circleOptions = FALSE,     # Disable circle
        markerOptions = FALSE,     # Disable marker
        polylineOptions = FALSE,   # Disable polylines
        circleMarkerOptions = FALSE, # Disable circle markers
        editOptions = editToolbarOptions()
      ) %>%
    addLayersControl(
        overlayGroups = "Danube Basin",  # Add the Danube Basin layer
        options = layersControlOptions(collapsed = FALSE)  # Show control options by default
      )
  })
  
  # Observe drawing events to capture polygon or rectangle
  observeEvent(input$map_draw_new_feature, {
    shape_coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    
    # Create an sf polygon or rectangle from the coordinates
    shape <- st_polygon(list(matrix(unlist(shape_coords), ncol = 2, byrow = TRUE))) %>%
      st_sfc(crs = 4326) %>%
      st_sf()
    
    drawn_shape(shape)  # Store the shape in reactive value
  })
  
  # Observe when shapes are deleted or cleared, and reset the map and table
  observeEvent(input$map_draw_deleted_features, {
    drawn_shape(NULL)  # Reset shape when shapes are cleared
    
    # Reset the map to show all points
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = points_sf,
                 ~st_coordinates(geometry)[, 1], 
                 ~st_coordinates(geometry)[, 2],
                 popup = ~paste0("<strong>Species:</strong> ", scientificName, 
                                 "<br><strong>Year:</strong> ", year, 
                                 "<br><strong>Location:</strong> ", locality,
                                 "<br><strong>Country:</strong> ", country,
                                 "<br><strong>Dataset:</strong> ", datasetName,
                                 "<br><strong>Contact Person:</strong> ", rightsHolder))
  })
  
  # Reactive expression to filter points on the map and data in the table
  filtered_data <- reactive({
    
    # Filter based on some fields of the table with species occurrences
    filtered_points <- points_sf %>%
      filter(scientificName %in% input$select_species) %>%
      filter(year >= input$slider_year[1] & year <= input$slider_year[2]) %>%
      filter(locality %in% input$select_locality) %>%
      filter(country %in% input$checkbox_country) %>%
      filter(rightsHolder %in% input$checkbox_contact)
    
    # Filter points inside polygon
    if (!is.null(drawn_shape())) {
      #return(points_sf)  # No shape drawn, return all points
    
    # Use st_within to find points inside the drawn shape
    points_inside <- st_within(points_sf, drawn_shape(), sparse = FALSE)
    filtered_points <- points_sf[points_inside, ]
    }
    filtered_points
  })
  
  # Observe changes in the filtered data to update the map with filtered points
  observe({
    
    leafletProxy("map") %>%
      clearMarkers()  # Clear existing markers
    
    # Check if there are any points to display
    if (nrow(filtered_data()) > 0) {
      # Add markers for points within the shape
      leafletProxy("map") %>%
        addMarkers(data = filtered_data(),
                   lng = ~st_coordinates(geometry)[, 1], 
                   lat = ~st_coordinates(geometry)[, 2],
                   popup = ~paste0("<strong>Species:</strong> ", scientificName, 
                                   "<br><strong>Year:</strong> ", year, 
                                   "<br><strong>Location:</strong> ", locality,
                                   "<br><strong>Country:</strong> ", country,
                                   "<br><strong>Dataset:</strong> ", datasetName,
                                   "<br><strong>Contact Person:</strong> ", rightsHolder))
    }
    
    # Changes produced drawing a polygon
    if (!is.null(drawn_shape())) {
      leafletProxy("map") %>%
        clearMarkers() %>%  # Clear existing markers
        # Add markers for points within the shape
        addMarkers(data = filtered_data(),
                   ~st_coordinates(geometry)[, 1], 
                   ~st_coordinates(geometry)[, 2],
                   popup = ~paste0("<strong>Species:</strong> ", scientificName, 
                                   "<br><strong>Year:</strong> ", year, 
                                   "<br><strong>Location:</strong> ", locality,
                                   "<br><strong>Country:</strong> ", country,
                                   "<br><strong>Dataset:</strong> ", datasetName,
                                   "<br><strong>Contact Person:</strong> ", rightsHolder))
    }
  })
  
  # Update the table with filtered points using DT with scrollable features
  output$filtered_points <- renderDT({
    filtered_data_df <- as.data.frame(filtered_data()) %>%
      dplyr::select(-geometry)  # Remove the geometry column
    
    # Add decimalLatitude and decimalLongitude back
    decimalLongitude <- points_data %>%
      filter(occurrenceID %in% filtered_data_df$occurrenceID) %>%
      pull(decimalLongitude)
    
    decimalLatitude <- points_data %>%
      filter(occurrenceID %in% filtered_data_df$occurrenceID) %>%
      pull(decimalLatitude)
    
    filtered_data_df <- filtered_data_df %>%
      mutate(decimalLongitude = decimalLongitude,
             decimalLatitude = decimalLatitude) %>%
      dplyr::select(all_of(names(points_data)))
    
    # Table
    datatable(filtered_data_df,
              options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE))  # Enable scroll bars
  })
  
  # Prepare dataset to be downloaded
  download_df <- reactive({
    filtered_data_df <- as.data.frame(filtered_data()) %>%
      dplyr::select(-geometry)  # Remove the geometry column
    
    # Add decimalLatitude and decimalLongitude back
    decimalLongitude <- points_data %>%
      filter(occurrenceID %in% filtered_data_df$occurrenceID) %>%
      pull(decimalLongitude)
    
    decimalLatitude <- points_data %>%
      filter(occurrenceID %in% filtered_data_df$occurrenceID) %>%
      pull(decimalLatitude)
    
    filtered_data_df <- filtered_data_df %>%
      mutate(decimalLongitude = decimalLongitude,
             decimalLatitude = decimalLatitude) %>%
      dplyr::select(all_of(names(points_data)))
  })
  
  # Download filtered data set
  output$download_occurr <- downloadHandler(
    filename = paste0("fish_danube_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(as.data.frame(download_df()), file)
    }
  )
  
  # Figures METRICS  tab
  # Number of records per species
  output$plot_species <- renderPlotly({
    species_count <- filtered_data() %>%
      st_drop_geometry() %>%
      group_by(scientificName) %>%
      summarise(occurrences = n())
    
    # Define a standard color palette using RColorBrewer
    colors_sp <- brewer.pal(n = nrow(species_count), name = "Set3")  # Choose a palette name and number of colors
    
    
    # Create the pie chart
    plot_ly(
      species_count,
      labels = ~scientificName,
      values = ~occurrences,
      type = 'pie',
      textinfo = 'none', # Hide labels
      insidetextorientation = 'radial',
      marker = list(colors = colors_sp)  # Assign standard colors
    ) %>% 
      layout(
        title = "",
        showlegend = FALSE # Hide legend if not needed
      )
  })
  
  # Number of records per year
  output$plot_year <- renderPlotly({
    # Count occurrences per year
    year_count <- filtered_data() %>%
      st_drop_geometry() %>%
      group_by(year) %>%
      summarise(occurrences = n())
    
    # Create a time series plot
    plot_ly(
      data = year_count,
      x = ~year,
      y = ~occurrences,
      type = 'scatter',
      mode = 'lines+markers', # for a line plot with markers
      line = list(shape = 'linear', color = '#1f77b4')
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Occurrences"),
        showlegend = FALSE
      )
  })
  
  # Number of records per country
  output$plot_country <- renderPlotly({
    country_count <- filtered_data() %>%
      st_drop_geometry() %>%
      group_by(country) %>%
      summarise(occurrences = n())
    
    # Define a standard color palette using RColorBrewer
    colors_country <- brewer.pal(n = nrow(country_count), name = "Set3")  # Choose a palette name and number of colors
    
    # Create the pie chart
    plot_ly(
      country_count,
      labels = ~country,
      values = ~occurrences,
      type = 'pie',
      textinfo = 'none', # Hide labels
      insidetextorientation = 'radial',
      marker = list(colors = colors_country)  # Assign standard colors
    ) %>% 
      layout(
        title = "",
        showlegend = FALSE # Hide legend if not needed
      )
  })
  
  # Number of records per dataset
  output$plot_dataset <- renderPlotly({
    dataset_count <- filtered_data() %>%
      st_drop_geometry() %>%
      group_by(datasetName) %>%
      summarise(occurrences = n())
    
    # Define a standard color palette using RColorBrewer
    colors_dataset <- brewer.pal(n = nrow(dataset_count), name = "Set3")  # Choose a palette name and number of colors
    
    # Create the pie chart
    plot_ly(
      dataset_count,
      labels = ~datasetName,
      values = ~occurrences,
      type = 'pie',
      textinfo = 'none', # Hide labels
      insidetextorientation = 'radial',
      marker = list(colors = colors_dataset)  # Assign standard colors
    ) %>% 
      layout(
        title = "",
        showlegend = FALSE # Hide legend if not needed
      )
  })
  
  
}

# Run the Shiny app
shinyApp(ui, server)
