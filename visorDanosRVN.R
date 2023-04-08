# PAQUETES

library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinydashboard)


# DATOS

# Capa de polígonos: zonas de conservación vial
zonas <-
  st_read(
    "https://raw.githubusercontent.com/ggaltar/danos_red_vial/main/capas/zonas_conservacion_wgs84.geojson",
    quiet = TRUE
  )

# Capa de líneas: red vial nacional
rutas <-
  st_read(
    "https://raw.githubusercontent.com/ggaltar/danos_red_vial/main/capas/red_vial_nacional_wgs84.geojson",
    quiet = TRUE
  ) 

# Capa de puntos: daños en la red vial nacional
danos <-
  st_read(
    "https://firebasestorage.googleapis.com/v0/b/registros01-cd58a.appspot.com/o/capa%2Fdanos_wgs84.geojson?alt=media",
    quiet = TRUE
  )

# LISTAS PARA FILTROS

# Lista ordenada de estructuras + "Todas"
lista_estructuras <- unique(danos$estructura)
lista_estructuras <- sort(lista_estructuras)
lista_estructuras <- c("Todas", lista_estructuras)


# Lista ordenada de daños + "Todos"
lista_dano <- unique(danos$dano)
lista_dano <- sort(lista_dano)
lista_dano <- c("Todos", lista_dano)

# Lista ordenada de severidad + "Todas"
lista_severidad <- unique(danos$severidad)
lista_severidad <- c("Todas", lista_severidad)

# Lista ordenada de servicio + "Todos"
lista_servicio <- unique(danos$servicio)
lista_servicio <- c("Todos", lista_servicio)

# Lista ordenada de zonas + "Todas"
lista_zonas <- unique(danos$zona)
lista_zonas <- sort(lista_zonas)
lista_zonas <- c("Todas", lista_zonas)

# Lista ordenada de rutas + "Todas"
lista_rutas <- unique(danos$ruta)
lista_rutas <- sort(lista_rutas)
lista_rutas <- c("Todas", lista_rutas)

# COMPONENTES DE LA APLICACIÓN SHINY

# Definición del objeto ui

ui <- dashboardPage(
  dashboardHeader(title = "Daños en la RVN"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "estructura",
        label = "Estructura",
        choices = lista_estructuras,
        selected = "Todas"
      ),
      selectInput(
        inputId = "dano",
        label = "Daño",
        choices = lista_dano,
        selected = "Todos"
      ),
      selectInput(
        inputId = "severidad",
        label = "Severidad",
        choices = lista_severidad,
        selected = "Todas"
      ),
      selectInput(
        inputId = "servicio",
        label = "Servicio",
        choices = lista_servicio,
        selected = "Todos"
      ),
      selectInput(
        inputId = "zona",
        label = "Zona",
        choices = lista_zonas,
        selected = "Todas"
      ),
      selectInput(
        inputId = "ruta",
        label = "Ruta",
        choices = lista_rutas,
        selected = "Todas"
      ),
      dateRangeInput(
        inputId = "fecha",
        label = "Fecha",
        start = "2015-01-01",
        end   = Sys.Date(),
        separator = " a ",
        language = "es"
      ),
      startExpanded = TRUE
    )
  )),
  dashboardBody(fluidRow(
    box(
      title = "Mapa de ubicación de daños",
      leafletOutput(outputId = "mapa"),
      width = 12
    ),
  ),    
  fluidRow(
    box(
      title = "Registros de daños",
      DTOutput(outputId = "tabla"),
      width = 12
    ),
  ))
)

# Definición del objeto server

server <- function(input, output, session) {
  filtrarDanos <- reactive({
    # Remoción de geometrías y selección de columnas
    danos_filtrado <-
      danos %>%
      dplyr::select(estructura, elemento, dano, severidad, servicio, ruta, seccion, zona, disparador, fecha, observaciones, usuario, fecha_reporte, fotos)
    
    # Filtrado de daños por estructura
    if (input$estructura != "Todas") {
      danos_filtrado <-
        danos_filtrado %>%
        filter(estructura == input$estructura)
    }   
    # Filtrado de daños por tipo
    if (input$dano != "Todos") {
      danos_filtrado <-
        danos_filtrado %>%
        filter(dano == input$dano)
    }
    # Filtrado de daños por severidad
    if (input$severidad != "Todas") {
      danos_filtrado <-
        danos_filtrado %>%
        filter(severidad == input$severidad)
    }
    # Filtrado de daños por servicio
    if (input$servicio != "Todos") {
      danos_filtrado <-
        danos_filtrado %>%
        filter(servicio == input$servicio)
    }
    # Filtrado de daños por zona
    if (input$zona != "Todas") {
      danos_filtrado <- danos %>%
        filter(zona == input$zona)
    }
    # Filtrado de daños por ruta
    if (input$ruta != "Todas") {
      danos_filtrado <- danos %>%
        filter(ruta == input$ruta)
    }
    # Filtrado de daños por fecha
    danos_filtrado <-
      danos_filtrado %>%
      filter(
        fecha >= as.Date(input$fecha[1], origin = "1970-01-01") &
          fecha <= as.Date(input$fecha[2], origin = "1970-01-01")
      )
    return(danos_filtrado)
  })
  
  
  # Mapa ubicación de los daños
  
  output$mapa <- renderLeaflet({
    registros <- filtrarDanos()
    
    lista_colores <- c("#94d2bd","#94d2bd","#ee9b00","#ca6702","#bb3e03","#ae2012","#9b2226","#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c","#495057")
    
    # Registro de daños, zonas de conservación y ráster de zonas de conservación por cantidad
    leaflet() %>%
      setView(-84.08, 9.83, 8) %>%
      addProviderTiles(providers$CartoDB.Voyager , group = "Voyager") %>%
      addTiles(group = "OSM") %>%
      addPolygons(
        data = zonas,
        color = "#38302e",
        fillColor = "transparent",
        stroke = TRUE,
        weight = 3,
        opacity = 1,
        group = "Zonas de conservación"
      ) %>%    
      addPolylines(
        data = rutas,
        color = "#788585",
        fill = FALSE,
        stroke = TRUE,
        weight = 1,
        opacity = 1,
        group = "Red vial nacional",
        label = paste0(
          "Ruta: ", rutas$ruta
        )
      ) %>%
      
      addCircleMarkers(
        data = registros,
        stroke = F,
        radius = 3,
        fillColor = '#d62828',
        fillOpacity = 1,
        group = "Daños",
        label = paste0(
          "Estructura: ", registros$estructura,
          ", ",
          "Elemento: ", registros$elemento,
          ", ",
          "Daño: ", registros$dano
        ),
        popup = (paste0(
          "<strong>Estructura: </strong>", registros$estructura,
          "<br>",
          "<strong>Elemento: </strong>", registros$elemento,
          "<br>",
          "<strong>Daño: </strong>", registros$dano,
          "<br>",
          "<strong>Severidad: </strong>", registros$severidad,
          "<br>",
          "<strong>Servicio: </strong>", registros$servicio,
          "<br>",
          "<strong>Ruta: </strong>", registros$ruta,
          "<br>",
          "<strong>Sección de control: </strong>", registros$seccion,
          "<br>",
          "<strong>Zona de conservación: </strong>", registros$zona,
          "<br>",
          "<strong>Evento disparador: </strong>", registros$disparador,
          "<br>",
          "<strong>Fecha del evento (año-mes-día): </strong>", registros$fecha,
          "<br>",
          "<strong>Observaciones: </strong>", registros$observaciones,
          "<br>",
          "<strong>Usuario: </strong>", registros$usuario,
          "<br>",
          "<strong>Fecha del reporte (año-mes-día): </strong>", registros$fecha_reporte,
          "<br>",
          "<strong>Fotos:</strong><br><a>", registros$fotos,"</a>",
          "<br>")
        )
      )  %>%
      
      addLayersControl(
        baseGroups = c("Voyager", "OSM"),
        overlayGroups = c("Daños", "Red vial nacional", "Zonas de conservación"),
        options = layersControlOptions(collapsed = T)
      ) %>%
      
      hideGroup("Zonas de conservación 2") %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      addMouseCoordinates()
  })
  
  # Tabla de registro de daños
  
  output$tabla <- renderDT({
    registros <- filtrarDanos()
    registros %>%
      dplyr::select(estructura,elemento, dano, severidad, servicio, fecha, ruta, seccion) %>%
      st_drop_geometry() %>%
      
      datatable(rownames = FALSE,
                colnames = c('Estructura','Elemento', 'Daño', 'Severidad','Servicio', 'Fecha', 'Ruta', 'Sección'),
                options = list(
                  pageLength = 7,
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                )
      )
  })
}

shinyApp(ui, server)