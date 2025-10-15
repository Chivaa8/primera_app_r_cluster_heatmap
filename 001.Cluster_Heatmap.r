# app.R
library(shiny)
library(pheatmap)   # para generar el cluster heatmap
library(DT)         # para mostrar tablas interactivas


#INTERFAZ DE USUARIO

ui <- fluidPage(
  titlePanel("Cluster Heatmap"),
  
  sidebarLayout(
    sidebarPanel(
      # Carga de archivo 
      fileInput("archivo", "Sube tu matriz de expresión (CSV)", 
                accept = c(".csv")), 
      
      # Opción para escalar los datos
      checkboxInput("escalar", "Escalar filas (Z-score)", value = TRUE),
      
      # Selector de color
      selectInput("paleta", "Paleta de colores:", 
                  choices = c("Verde-Rojo" = "greenred",
                              "Azul-Blanco-Rojo" = "bwr",
                              "Azul-Amarillo-Rojo" = "ayr")),
      
      actionButton("procesar", "Generar Heatmap")
    ),
    
    mainPanel(
      h3("Vista previa de los datos"),
      DTOutput("tabla"), #muestra una tabla interactiva
      br(),
      h3("Cluster Heatmap"),
      plotOutput("heatmap", height = "600px")
    )
  )
)


#LÓGICA DEL SERVIDOR

server <- function(input, output) {
  
  # Cuando se pulsa el botón "procesar" genera el gráfico
  observeEvent(input$procesar, { 
    
    # Leer el archivo CSV subido
    datos <- reactive({
      req(input$archivo)  # asegura que haya archivo
      read.csv(input$archivo$datapath, row.names = 1)
    })
    
    # Mostrar la tabla
    output$tabla <- renderDT({
      head(datos(), 10)
    })
    
    # Renderizar el heatmap
    output$heatmap <- renderPlot({
      req(datos())  # asegura que existan datos
      
      matriz <- as.matrix(datos())
      
      # Escalado opcional (Z-score por fila)
      if (input$escalar) {
        matriz <- t(scale(t(matriz)))
      }
      
      # Selección de paleta
      colores <- switch(input$paleta,
                        "greenred" = colorRampPalette(c("green", "black", "red"))(100), #genera los tonos de colores
                        "bwr" = colorRampPalette(c("blue", "white", "red"))(100),
                        "ayr" = colorRampPalette(c("blue", "yellow", "red"))(100)
      )
      
      # Generar el heatmap con clustering
      pheatmap(matriz, 
               color = colores,
               clustering_distance_rows = "euclidean",
               clustering_distance_cols = "euclidean",
               clustering_method = "complete",
               main = "Cluster Heatmap de expresión génica")
    })
  })
}


#EJECUTAR LA APP

shinyApp(ui = ui, server = server)
