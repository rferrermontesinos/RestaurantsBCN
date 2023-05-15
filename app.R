library(raster)
library(sf)
library(leaflet) #for making the interactive map
library(rgdal) #for importing vector data
library(htmlwidgets) # export html
library(tidyverse)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(leaflet.extras)



shp = st_read("Shapes/restBCN.shp")
crs(shp)
new_crs <- '+init=epsg:4326' # the code for our new projection
shp_proj <- st_transform(shp, CRS(new_crs))
crs(shp_proj)

# shp_limit = st_read(dsn= "Shapes/tota_ciutat_ETRS89.shp")
shp_limit = st_read(dsn= "Shapes/Districtes.shp")

crs(shp_limit)
shp_limit_proj <- st_transform(shp_limit, CRS(new_crs))
crs(shp_limit_proj)
shp_limit_proj_ext <- st_bbox(shp_limit_proj)
bbox_list_bcn <- as.list(shp_limit_proj_ext) #listar xmax ymin xmin ymin
# setviewX <- bbox_list$xmin + bbox_list$xmax / 2
# setviewY <- bbox_list$ymin + bbox_list$ymax / 2

# colors_map <- c("PSOE" = "#EE6450",
#    "PP" = "#A4D3EE",
#    "VOX" = "#90EE90",
#    "PODEMOS" = "#EE7AE9",
#    "Cs" = "#EE9A00",
#    "ERC" = "#EEEE00", 
#    "JxCAT" = "#4a708b",
#    "Bildu" = "#70E0D0",
#    "CCa" = 'white',
#    "Compromis" = "#D2B48C",
#    "Na+" = "#6C7B8B",
#    "OTROS" = 'white',
#    "PNV" = "#698B22",
#    "PRC" = "#ffffe0")

# pal <- colorFactor(c("green", "red"), domain = c("Actiu", "Sense activitat Econòmica"))


pal <- colorFactor(c("#d81159", "#218380", "#73d2de", "#ffbc42", "#8f2d56"),
                   domain = c("Restaurant",
                              "Bar / Wifi",
                              "Servei de menjar take away",
                              "Bar musical / Discoteca / PUB",
                              "Servei de menjar i begudes"))

# UI

ui <- fluidPage(
  tags$style(type="text/css",
             ".leaflet {height:100%;width:100%} ",
             ".leaflet-top {z-index:9999 !important; }",
             ".leaflet-bottom {z-index:9999 !important; }"
  ),
  leafletOutput(outputId = "map", height = "100vh"),
  absolutePanel(
    draggable = TRUE,
    class= "collapse in",
    top = 50,
    left = 50,
    right = NULL,
    bottom = NULL,
    width = 340,
    height = "auto",
    style = "background-color: white; 
            font-family: Arial;
            opacity: 0.9;
            padding:15px;
            box-shadow: 2px 2px 4px rgba(0,0,0,0.2);",
    tags$img(src = "rmaps_logo.png",
             width = "100%", 
             height = "100%", 
             style = "max-height: 150px;"),
    fluidRow(
      br(),
      column(12, h4("Restaurants a Barcelona", align="center",style="font-weight: bold;")),
      column(12,
             tags$hr(),
             pickerInput(
               inputId = "tipus",
               label = "Tipus",
               choices = c("Restaurant", 
                           "Bar / Wifi", 
                           "Servei de menjar take away",
                           "Bar musical / Discoteca / PUB",
                           "Servei de menjar i begudes"
                           ),
               multiple = TRUE,
               selected = "NULL",
               options = list(
                `none-selected-text` = "Selecciona...")
             ),
             pickerInput(
               inputId = "zona",
               label = "Zona",
               choices = unique(shp_limit_proj$NOM)[!is.na(unique(shp_limit_proj$NOM))],
               multiple = TRUE,
               selected = "NULL",
               options = list(
                          `max-options` = 1,
                          `none-selected-text` = "Selecciona...")
             ),
             tags$span(textOutput("n_resultats"), align = "center"),
             br(),
             div(style = "text-align:center;",
                 actionBttn(
                   inputId = "cluster",
                   label = "Clúster",
                   color = "warning",
                   style = "unite",
                   size = "sm",
                   block = FALSE
                 ),
                 actionBttn(
                   inputId = "heatmap",
                   label = "Mapa de calor",
                   color = "success",
                   style = "unite",
                   size = "sm",
                   block = FALSE
                 ),
             )
             # downloadBttn(
             #   outputId = "downloadInforme",
             #   label = "Informe",
             #   color = "success",
             #   style = "unite",
             #   size = "sm",
             #   block = FALSE,
             #   icon = shiny::icon("download")
             # ),
             # downloadButton("downloadInforme","Informe")
      )
    )
  )
)


# ui <- dashboardPage(
#   skin = "black",
#   dashboardHeader(
#     title = "Cens de Comerç a Castelldefels",
#     titleWidth = 350),
#   dashboardSidebar(
#     width = 350,
#     # Filtre ESTAT
#     pickerInput(inputId = "stat",
#                 label = "Estat",
#                 choices = c("Inactiu", "Actiu", "Sense informació"),
#                 multiple = TRUE,
#                 selected = "NULL",
#                 options = list(
#                   # `actions-box` = TRUE,
#                   # `deselect-all-text` = "Cap",
#                   # `select-all-text` = "Tots",
#                   `none-selected-text` = "Selecciona..."
#                 )
#     ),
#     # Filtro SECTOR
#       pickerInput(
#         inputId = "sector",
#         label = "Sector",
#         choices = unique(shp_proj$Nom_Sector)[!is.na(unique(shp_proj$Nom_Sector))],
#         multiple = TRUE,
#         selected = NULL,
#         options = list(
#           `none-selected-text` = "Selecciona..."
#         )
#       ),
#       actionBttn(
#         inputId = "cluster",
#         label = "Clúster",
#         color = "warning",
#         style = "unite",
#         size = "sm",
#         block = FALSE
#       ),
#       actionBttn(
#         inputId = "heatmap",
#         label = "Mapa de calor",
#         color = "warning",
#         style = "unite",
#         size = "sm",
#         block = FALSE
#       ),
#     # actionButton(inputId = "heatmap", label = "Mapa de calor", icon = icon("fire")),
#     #  actionButton(inputId = "cluster", label = "Clúster", icon = icon("map-marker-alt"))
#     downloadButton("informe","Descarrega")
#   ),
#   dashboardBody(
#     fluidRow(
#       box(width = 12, status = "primary", 
#           leafletOutput(outputId = "map", height = "100vh"),
#           
#       )
#     )
#   )
# )

# Server
server <- function(input, output, session) {
  
  labels <- reactive({
    filtered_data <- filteredData()
    paste0(
      "<strong>",filtered_data$Nom_Local,"</strong>","<br/>",
      filtered_data$Nom_Via, ", ",
      filtered_data$Num_Polici,
      " (",filtered_data$Nom_Barri,")","<br/>",
      filtered_data$Nom_Activi,"<br/>",
      "Estat: ", filtered_data$Nom_Princi
    ) %>% lapply(htmltools::HTML)
  })
  
  filteredData <- reactive({
    filtered_data <- shp_proj
    
    if (!is.null(input$tipus)) {
      filtered_data <- filtered_data[filtered_data$Nom_Activi %in% input$tipus, ]
    }
    
    if (!is.null(input$zona)) {
      filtered_data <- filtered_data[filtered_data$Nom_Distri %in% input$zona, ]
    }
    
    if (is.null(input$tipus) && is.null(input$zona)) {
      filtered_data <- shp_proj
    }
    
    return(filtered_data)

  })
  
  ##################################################
  #### FIJAR LA VISTA Y ZOOM EN FUNCION DEL INPUT
  ##################################################
  
  setviewX <- reactive({
    
    limitDataX <- shp_limit_proj
    
    if (!is.null(input$zona)) {
      limitDataX <- limitDataX[limitDataX$NOM == input$zona, ]
      limitDataXBB <- st_bbox(limitDataX)
      bbox_list_limit <- as.list(limitDataXBB) # cambiar a bbox_list_limit
      setviewX <- (bbox_list_limit$xmin + bbox_list_limit$xmax) / 2 # corregir el cálculo
    } else {
    shp_limit_proj_ext <- st_bbox(shp_limit_proj)
    bbox_list <- as.list(shp_limit_proj_ext) #listar xmax ymin xmin ymin
    setviewX <- (bbox_list_bcn$xmin + bbox_list_bcn$xmax) / 2
    }
    return (setviewX)
  })


  setviewY <- reactive({
    
    limitDataY <- shp_limit_proj
    
    if (!is.null(input$zona)) {
      limitDataY <- limitDataY[limitDataY$NOM %in% input$zona, ]
      limitDataYBB <- st_bbox(limitDataY)
      bbox_list_limit <- as.list(limitDataYBB) # cambiar a bbox_list_limit
      setviewY <- (bbox_list_limit$ymin + bbox_list_limit$ymax) / 2 # corregir el cálculo
    } else {
      shp_limit_proj_ext <- st_bbox(shp_limit_proj)
      bbox_list <- as.list(shp_limit_proj_ext) #listar xmax ymin xmin ymin
      setviewY <- (bbox_list_bcn$ymin + bbox_list_bcn$ymax) / 2
    }
    
    return (setviewY)
  
  })
  
  zoomZona <- reactive({
    
    if (!is.null(input$zona)) {
      zoomZona = 15
    } else {
      zoomZona = 13
    }
    
    return (zoomZona)
    
  })
  
  distPoly <- reactive({
    
    shp_limit_proj_dist <- shp_limit_proj
    
    if (!is.null(input$zona)) {
      shp_limit_proj_dist <- shp_limit_proj_dist[shp_limit_proj_dist$NOM %in% input$zona, ]
    } else {
      shp_limit_proj_dist <- shp_limit_proj
      
    }
    
    return (shp_limit_proj_dist)
    
  })
  
  ##################################################
  #### FIJAR LA VISTA Y ZOOM EN FUNCION DEL INPUT
  ##################################################
  
  
  
  
  
  #####NUMERO DE RESULTATS#####
  # n_resultats <- reactive({nrow(filteredData())
  # })
  # output$n_resultats <- renderText("Número de resultats: ", n_resultats())
  output$n_resultats <- renderText({
    paste("Número de resultats: ", nrow(filteredData()))
  })
  #####NUMERO DE RESULTATS#####
  
 
  
  ########################################
  ### OBSERVE SI APRETA A CLUSTER Y HAY ZONA SELECCIONADA
  ##################################################
  
  observeEvent(input$cluster, {
    filtered <- filteredData()
    zoom <- zoomZona()
    viewX <- setviewX()
    viewY <- setviewY()
    output$map <- renderLeaflet({
      map <- leaflet(filtered, options = leafletOptions(attributionControl = FALSE,
                                                        minZoom = 12,
                                                        zoomControl = FALSE,
      )) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(lng = viewX, lat = viewY, zoom = zoom) %>%
        addCircleMarkers(
          group = "Clúster",
          fillColor = ~pal(Nom_Activi),
          fillOpacity = 0.9,
          stroke = FALSE,
          radius = 6,
          color = "blue",
          clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                                spiderfyDistanceMultiplier = 1.5,
                                                disableClusteringAtZoom = 17),
          label = labels(),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "14px",
            direction = "auto"
          )
        )%>%
        clearControls()%>%
        addLegend(data= shp_proj, # the dataset
                  "bottomright", # where to put the legend
                  pal=pal, values = ~Nom_Activi, # specify the color palette and the range of values
                  title="Tipus d'activitat", # legend title
                  opacity = 1.0) # the transparency of the legend      
       if (!is.null(input$zona)) {
        map <- map %>% addPolygons(data = distPoly(),
                                   color = "black", #color del borde
                                   weight = 0.5,
                                   opacity = 0.5,
                                   fillColor = "grey80",
                                   fillOpacity = 0.05)
      }
      
      return(map)
    })
  })
  
  observeEvent(input$heatmap, {
    filteredDataUpdated <- filteredData()
    output$map <- renderLeaflet({
      map <- leaflet(filteredData(), options = leafletOptions(attributionControl = FALSE,
                                                              minZoom = 12,
                                                              zoomControl = FALSE)) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        # setMaxBounds(lng1 = bbox_list_bcn$xmin, lat1 = bbox_list_bcn$ymin, lng2 = bbox_list_bcn$xmax, lat2 = bbox_list_bcn$ymax)%>%
        # setView(lng = 1.98, lat = 41.27, zoom = 14) %>%
        setView(lng = setviewX(), lat = setviewY(), zoomZona()) %>%
        addHeatmap(data=filteredDataUpdated,
                   group="Mapa de calor",
                   radius = 5, blur = 10,
                   minOpacity = 0.1,
                   max = 0.5)
      if (!is.null(input$zona)) {
        map <- map %>% addPolygons(data = distPoly(),
                                   color = "black", #color del borde
                                   weight = 0.5,
                                   opacity = 0.5,
                                   fillColor = "grey80",
                                   fillOpacity = 0.1)
      }
      
      return(map)
    })
  })


   
   current_view <- reactiveVal("markers") # Inicialmente se muestra el mapa de marcadores
   
   observeEvent(input$cluster, {
     current_view("markers")
     # Código para mostrar los marcadores de círculo
   })
   
   observeEvent(input$heatmap, {
     current_view("heatmap")
     # Código para mostrar el mapa de calor
   })
   
   
   

   # #### OUTPUT PDF#####
   # 
   # output$downloadInforme <- downloadHandler(
   #   
   #   # For PDF output, change this to "report.pdf"
   #   filename = "report.html",
   #   content = function(file) {
   #     # Copy the report file to a temporary directory before processing it, in
   #     # case we don't have write permissions to the current working dir (which
   #     # can happen when deployed).
   #     tempReport <- file.path(tempdir(), "informe.Rmd")
   #     file.copy("informe.Rmd", tempReport, overwrite = TRUE)
   #     
   #     # Set up parameters to pass to Rmd document
   #     params <- list(n = input$stat)
   #     
   #     # Knit the document, passing in the `params` list, and eval it in a
   #     # child of the global environment (this isolates the code in the document
   #     # from the code in this app).
   #     rmarkdown::render(tempReport, output_file = file,
   #                       params = params,
   #                       envir = new.env(parent = globalenv())
   #     )
   #   }
   # )
   # 
   # 
   # #### OUTPUT #####
   

  output$map <- renderLeaflet({
    leaflet(filteredData(), options = leafletOptions(attributionControl = FALSE, 
                                                     minZoom = 12,
                                                     zoomControl = FALSE, # desactiva los botones de zoom predeterminados
                                                     )) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      # setView(lng = 1.98, lat = 41.27, zoom = 14) %>%
      setView(lng = setviewX(), lat = setviewY(), zoomZona()) %>%
      # setMaxBounds(lng1 = bbox_list_bcn$xmin, lat1 = bbox_list_bcn$ymin, lng2 = bbox_list_bcn$xmax, lat2 = bbox_list_bcn$ymax)%>%
      addCircleMarkers(
        group = "Clúster",
        fillColor = ~pal(Nom_Activi),
        fillOpacity = 0.9,
        stroke = FALSE,
        radius = 6,
        color = "blue",
        clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                              spiderfyDistanceMultiplier = 1.5,
                                              disableClusteringAtZoom = 17),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "14px",
          direction = "auto"
        )
      )
  })

   observe({
     filteredDataUpdated <- filteredData()
     if (current_view() == "markers") {
       # Código para mostrar los marcadores de círculo
       leafletProxy("map", session = session) %>%
         clearMarkers() %>%
         addCircleMarkers(
           data = filteredDataUpdated,
           group = "Clúster",
           fillColor = ~pal(Nom_Activi),
           fillOpacity = 0.9,
           stroke = FALSE,
           radius = 6,
           clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                                 spiderfyDistanceMultiplier = 1.5,
                                                 disableClusteringAtZoom = 17),
           label = labels(),
           labelOptions = labelOptions(
             style = list("font-weight" = "normal", padding = "3px 8px"),
             textsize = "14px",
             direction = "auto"
           )
         )%>%
         clearControls()%>%
         addLegend(data= shp_proj, # the dataset
                   "bottomright", # where to put the legend
                   pal=pal, values = ~Nom_Activi, # specify the color palette and the range of values
                   title="Tipus d'activitat", # legend title
                   opacity = 1.0) # the transparency of the legend
     } else {
       # Código para mostrar el mapa de calor
       leafletProxy("map", session = session) %>%
         clearHeatmap() %>%
         addHeatmap(data = filteredDataUpdated,
                    group = "Mapa de calor",
                    radius = 5, blur = 10,
                    minOpacity = 0.1,
                    max = 0.5
         )
     }
   })
   
   observe({
   if (!is.null(input$zona)) {
     output$map <- renderLeaflet({
       leaflet(filteredData(), options = leafletOptions(attributionControl = FALSE, 
                                                        minZoom = 12,
                                                        zoomControl = FALSE,
       )) %>%
         addProviderTiles(provider = "CartoDB.Positron") %>%
         # setView(lng = 1.98, lat = 41.27, zoom = 14) %>%
         setView(lng = setviewX(), lat = setviewY(), zoomZona()) %>%
         addPolygons(data = distPoly(),
                     color = "black", #color del borde
                     weight = 0.5,
                     opacity = 0.5,
                     fillColor = "grey80",
                     fillOpacity = 0.1
         )
     })
   }
     else {
       output$map <- renderLeaflet({
         leaflet(filteredData(), options = leafletOptions(attributionControl = FALSE, 
                                                          minZoom = 12,
                                                          zoomControl = FALSE,
         )) %>%
           addProviderTiles(provider = "CartoDB.Positron") %>%
           # setView(lng = 1.98, lat = 41.27, zoom = 14) %>%
           setView(lng = setviewX(), lat = setviewY(), zoomZona())
           })
      }
   }) 
   
} 

shinyApp(ui, server)


