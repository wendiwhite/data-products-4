library(shiny)
library(leaflet)
library(RColorBrewer)

pal <- colorNumeric("Reds", c(min(quakes$mag)-0.5, max(quakes$mag)))


ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(top = 10, right = 10,
                      sliderInput("mags", "Magnitudes", min(quakes$mag), max(quakes$mag),
                                  value = range(quakes$mag), step = 0.1
                      ),
                      sliderInput("depths", "Depths", min(quakes$depth), max(quakes$depth),
                                  value = range(quakes$mag), step = 1
                      ),
                      checkboxInput("legend", "Show legend", TRUE),
                      htmlOutput("summary"),
                      plotOutput("plot"),
                      style = "background: rgba(255, 255, 255, 0.5); padding: 5px; border: 1px solid black"
        )
)

server <- function(input, output, session) {
        
        filteredData <- reactive({
                quakes[quakes$mag >= input$mags[1] & quakes$mag <= input$mags[2] & quakes$depth >= input$depths[1] & quakes$depth <= input$depths[2],]
        })

        output$map <- renderLeaflet({
                leaflet(quakes) %>% addTiles() %>%
                        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
        })
        
        output$summary <- renderText({
                paste("<label>Selected Data Summary</label><p>Average depth: ", round(mean(filteredData()$depth),2), " km<br>",
                      "Average magnitude:", round(mean(filteredData()$mag),1)," Richter</p>")
        })
        
        output$plot <- renderPlot({
                plot(filteredData(), pch=20, lwd=.2, cex=.2)
        })
        
        observe({
                leafletProxy("map", data = filteredData()) %>%
                        clearShapes() %>%
                        addCircles(radius = ~10^mag/10,
                                   weight = 1,
                                   color = "#777777",
                                   fillColor = ~pal(mag),
                                   fillOpacity = 0.7,
                                   popup = ~paste0("<label>Event details</label><div>Magnitude: ", mag, " Richter<br>Depth: ", depth, " km</div>")
                        )
        })
        
        observe({
                proxy <- leafletProxy("map", data = quakes)
                
                proxy %>% clearControls()
                if (input$legend) {
                        proxy %>% addLegend(position = "bottomright",
                                            pal = pal,
                                            values = ~mag,
                                            title = "Richter"
                        )
                }
        })
}

shinyApp(ui, server)