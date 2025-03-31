# TASK : Develop Real time bus tracking system where live gps coordinates of buses are streamed on google maps on backend, 
# stimulates buses going on predefined routes on front end - updates the maps every few seconds 

# GOKULAKRISHNAN. V


library(shiny)
library(leaflet)

# Route for Bus 1
route1 <- data.frame(
  lat = c(
    12.968612, 12.969487, 12.969692, 12.969777, 12.971149, 
    12.971866, 12.972464, 12.972716, 12.972309, 12.971736, 
    12.971172, 12.968713, 12.968765, 12.968612
  ),
  lng = c(
    79.156297, 79.155315, 79.156617, 79.158646, 79.159496, 
    79.163627, 79.166199, 79.167526, 79.166202, 79.163437, 
    79.159862, 79.158673, 79.157501, 79.156297
  ),
  stringsAsFactors = FALSE
)

# Route for Bus 2 
route2 <- data.frame(
  lat = c(
    12.972716, 12.972309, 12.971736, 12.971172, 12.968713,
    12.968765, 12.968612, 12.969487, 12.969692, 12.969777,
    12.971149, 12.971866, 12.972464
  ),
  lng = c(
    79.167526, 79.166202, 79.163437, 79.159862, 79.158673,
    79.157501, 79.156297, 79.155315, 79.156617, 79.158646,
    79.159496, 79.163627, 79.166199
  ),
  stringsAsFactors = FALSE
)

current_position1 <- 1
current_position2 <- 1

# Function to update Bus 1 position
update_bus_position1 <- function() {
  if (current_position1 < nrow(route1)) {
    current_position1 <<- current_position1 + 1
  } else {
    current_position1 <<- 1
  }
  route1[current_position1, ]
}

# Function to update Bus 2 position
update_bus_position2 <- function() {
  if (current_position2 < nrow(route2)) {
    current_position2 <<- current_position2 + 1
  } else {
    current_position2 <<- 1
  }
  route2[current_position2, ]
}


ui <- fluidPage(
  titlePanel("Real-Time Bus Tracking (Two Buses) around VIT Vellore Campus"),
  p("GOKULAKRISHNAN. V - 22BCE3752"),
  leafletOutput("map", width = "100%", height = "600px")
)

server <- function(input, output, session) {
  
  google_tile_url <- "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}&key=YOUR_API_KEY"
  
  bus1_icon <- awesomeIcons(
    icon = "bus",
    library = "fa",
    markerColor = "blue"
  )
  bus2_icon <- awesomeIcons(
    icon = "bus",
    library = "fa",
    markerColor = "green"
  )
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = google_tile_url,
               options = tileOptions(maxZoom = 20)) %>%
      setView(lng = route1[1, "lng"], 
              lat = route1[1, "lat"], 
              zoom = 17) %>%
      
      addAwesomeMarkers(
        lng = route1[1, "lng"], 
        lat = route1[1, "lat"],
        layerId = "bus1_marker",
        icon = bus1_icon,
        popup = "Bus 1 Starting Position"
      ) %>%
      addAwesomeMarkers(
        lng = route2[1, "lng"], 
        lat = route2[1, "lat"],
        layerId = "bus2_marker",
        icon = bus2_icon,
        popup = "Bus 2 Starting Position"
      )
  })
  
  # Observe block to update both bus positions every few seconds
  observe({
    invalidateLater(3000, session)  # Update positions every 3 seconds
    
    
    new_pos1 <- isolate(update_bus_position1())
    new_pos2 <- isolate(update_bus_position2())
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        lng = new_pos1$lng, 
        lat = new_pos1$lat, 
        layerId = "bus1_marker",
        icon = bus1_icon,
        popup = paste("Bus 1 Position:",
                      round(new_pos1$lat, 4), ",",
                      round(new_pos1$lng, 4))
      ) %>%
      addAwesomeMarkers(
        lng = new_pos2$lng, 
        lat = new_pos2$lat, 
        layerId = "bus2_marker",
        icon = bus2_icon,
        popup = paste("Bus 2 Position:",
                      round(new_pos2$lat, 4), ",",
                      round(new_pos2$lng, 4)) 
      )
  })
}

shinyApp(ui, server)
