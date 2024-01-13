# Load required libraries
library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(viridis)
library(viridisLite)

##open
url3 <- "https://raw.githubusercontent.com/aacespedes/StingleeBeeBol.github.io/main/data/Meli_Shiny.csv"
bee_data <- read.csv(url3,  header = TRUE, sep = ";") 





# Define the UI
ui <- fluidPage(
  titlePanel("Stingless Bee Distribution Map for Bolivia"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Bee Species:",
                  choices = unique(bee_data$Species)),
      h4 ("Credits"),
      p("Cespedes-Llave, A.A."),
      p("Carrera de Biología - Facultad de Ciencias Químico Farmaceuticas y Bioquímicas"),
      p("Universidad San Francisco Xavier de Chuquisaca, Bolivia"),
      p("Sociedad Boliviana de Entomologia (SBE)"),
      p ("email: cespedes.ariel@usfx.bo"), 
      p ("Actualización: 06/01/2024")
    ),
    mainPanel(
      leafletOutput("map"),
      plotlyOutput("barplot"),
      DTOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Filter data based on selected species
  filtered_data <- reactive({
    subset(bee_data, Species == input$species)
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = ~Species)
  })
  # Render the barplot with viridis color palette
  output$barplot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Ecoregion) %>%
      summarize(Count = n())
    
    plot <- ggplot(plot_data, aes(x = Ecoregion, y = Count, fill = Count)) +
      geom_bar(stat = "identity") +
      labs(x = "Ecoregion", y = "Number of Species", title = "Number of Species by Ecoregion") +
      scale_fill_viridis(option = "plasma") +  # Use viridis color palette
      theme_minimal()
    
    ggplotly(plot)
  })
  
  # Render the contingency table
  output$table <- renderDT({
    datatable(rownames = FALSE,
              extensions = 'Buttons', 
              filtered_data(), 
              options = list(pageLength = 10, 
                             columnDefs = list(list(visible=FALSE, targets=c(9, 10, 11))
                             )))
  })
  


}

# Run the Shiny app
shinyApp(ui, server)


