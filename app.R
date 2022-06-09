## LIBRARIES

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(viridis)
library(readr)
library(utils)
library(shiny)
library(bslib)
library(shinyjs)
library(RColorBrewer)
library(scales)
library(lattice)
library(DT)

## DATA BASES IN GOOGLE SHEETS

resultados <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRmnriJM9hEi2reZCp2C3p39BtNbRAA0QqA8cEhrJj1agAeg3YyeyNSjk7sO4Xc9ejOfJRizkmqRRR7/pub?gid=723802881&single=true&output=csv")

## SHAPE FILE
shp_seccion <- read_sf(dsn = "seccionHidalgo", layer = "SeccionHidalgo")


## LEFT JOIN DATA BASE AND THE SHAPEFILE

shp_seccion <- resultados %>%
  dplyr::select(-MUNICIPIO) %>% 
  left_join(shp_seccion, ., by = 'seccion')

#### UI ####


ui <- (
  # Choices for drop-downs
  navbarPage(h4("INNOVA", style='color:#B3282D'), 
             id="nav",
             tabPanel("Innova", 
                      div(class="outer",
                          # Include our custom CSS
                          tags$head(includeCSS("styles.css") ),
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                        draggable = TRUE, top = 60, left = 40, right = "auto", bottom = "auto",
                                        width = 260, height = "auto",
                                        h3(img(src = "JM.jpg", height = 94, width = 186)),
                                        selectizeInput(inputId = "select_mun_1", label = "", selected = "MINERAL DE LA REFORMA", choices = sort(unique(na.omit(resultados$MUNICIPIO)))),
                                        uiOutput(outputId = 'select_sec_1'))
                      ) ),
              
             tabPanel("Resultados", h3(img(src = "JM.jpg", height = 94, width = 186)) , DT::dataTableOutput("ziptable"))
  ) ) 




#### SERVER ####

server <- function(input, output) {
  
  output$select_sec_1 <- renderUI({
    
    # check whether user wants to filter by cyl;
    # if not, then filter by selection
    if ('TODOS' %in% input$select_mun_1) {
      df <- resultados
    } else {
      df <- resultados %>%
        filter(
          MUNICIPIO %in% input$select_mun_1)
    }
    
    # get available carb values
    seccion_1 <- sort(unique(df$seccion))
    
    # render selectizeInput
    selectizeInput(
      inputId = 'select_sec_1',
      label = '',
      choices = c(input$select_mun_1, seccion_1),
      multiple = FALSE,
      selected = input$select_mun_1)
  })
  

  

    output$map <- renderLeaflet({
      tm <- shp_seccion %>% 
        filter(MunicipioN == input$select_sec_1) %>% 
        mutate(Diferencia = 100*(`JULIO MENCHACA` - `CAROLINA V`)/(`JULIO MENCHACA` + `CAROLINA V`)) %>% 
        tm_shape() +
        tm_polygons(col = "Diferencia", id = "seccion",  style = "fixed", breaks = c(-Inf,0,23.4,35.9,48.41,Inf), alpha = .5, palette = c("#3D31F6", "#EBC507" ,"#F59608" ,"#F24E0A", "#AA282D"), title = "Objetivo %", popup.vars = c("Julio M" = "JULIO MENCHACA", "Carolina V" = "CAROLINA V", "Francisco X" = "FRANCISCO X", "José Luis" = "JOSE LUIS"),  legend.show = FALSE) +
        tm_basemap("OpenStreetMap")
      tmap_leaflet(tm, in.shiny = TRUE)
    })
    

    output$ziptable <- DT::renderDataTable({
      tabla <- resultados 
      
      DT::datatable(tabla, options = list(pageLength = 200))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
