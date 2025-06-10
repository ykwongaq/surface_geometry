# Load a sample raster
path = file.path("higres_data", "SyntheticReef_500", "SyntheticReef_500.tif")
r <- raster(path)


# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Raster Click"),
  plotOutput("rasterPlot", click = "plot_click"),  # Capture click events
  verbatimTextOutput("click_info")                # Display click information
)

# Define the Server
server <- function(input, output, session) {
  
  # Render the raster plot
  output$rasterPlot <- renderPlot({
    plot(r, main = "Click on the raster to get coordinates")
  })
  
  # Display clicked coordinates
  output$click_info <- renderPrint({
    if (!is.null(input$plot_click)) {
      # Extract click information
      xy <- c(input$plot_click$x, input$plot_click$y)
      cat("Clicked coordinates:\n")
      print(xy)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)