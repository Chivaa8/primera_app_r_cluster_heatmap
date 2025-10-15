# app.R
library(shiny)
library(pheatmap)
library(DT)
library(ggplot2)


#INTERFAZ DE USUARIO

ui <- fluidPage(
  titlePanel("Cluster Heatmap"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube tu matriz de expresión (CSV)", accept = ".csv"),
      
      checkboxInput("escalar", "Escalar filas (Z-score)", value = TRUE),
      
      selectInput("paleta", "Paleta de colores:",
                  choices = c("Verde-Rojo" = "greenred",
                              "Azul-Blanco-Rojo" = "bwr",
                              "Azul-Amarillo-Rojo" = "ayr")),
      
      textInput("titulo", "Título del Heatmap:", 
                value = "Cluster Heatmap de expresión génica"),
      
      sliderInput("width", "Ancho (px):", min = 300, max = 6000, value = 800, step = 100),
      sliderInput("height", "Alto (px):", min = 300, max = 6000, value = 800, step = 100),
      sliderInput("res", "Resolución (dpi):", min = 72, max = 600, value = 150, step = 10),
      
      hr(),
      downloadButton("descargar_png", "Descargar PNG"),
      downloadButton("descargar_pdf", "Descargar PDF"),
      downloadButton("descargar_csv", "Descargar CSV"),
      
      br(), br(),
      actionButton("procesar", "Generar Heatmap")
    ),
    
    mainPanel(
      h3("Vista previa de los datos"),
      DTOutput("tabla"),
      br(),
      h3("Cluster Heatmap"),
      imageOutput("heatmap_zoom", height = "auto", width = "100%")
    )
  )
)

#LÓGICA DEL SERVIDOR

server <- function(input, output) {
  
  # Reactivo: leer archivo cuando se carga
  datos <- reactive({
    req(input$archivo)
    read.csv(input$archivo$datapath, row.names = 1)
  })
  
  # Mostrar tabla completa con navegación
  output$tabla <- renderDT({
    datatable(datos(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Función auxiliar para generar el heatmap y guardarlo si hace falta
  generar_heatmap <- function(file = NULL, format = "screen") {
    matriz <- as.matrix(datos())
    
    # Escalado opcional
    if (input$escalar) {
      matriz <- t(scale(t(matriz)))
    }
    
    # Paleta
    colores <- switch(input$paleta,
                      "greenred" = colorRampPalette(c("green", "black", "red"))(100),
                      "bwr" = colorRampPalette(c("blue", "white", "red"))(100),
                      "ayr" = colorRampPalette(c("blue", "yellow", "red"))(100))
    
    # Si se va a exportar a archivo, usar pheatmap normal
    if (!is.null(file)) {
      if (format == "pdf") {
        pdf(file, width = input$width/100, height = input$height/100)
      } else if (format == "png") {
        png(file, width = input$width, height = input$height, res = input$res)
      }
      pheatmap(matriz,
               color = colores,
               clustering_distance_rows = "euclidean",
               clustering_distance_cols = "euclidean",
               clustering_method = "complete",
               main = input$titulo)
      dev.off()
    } else {
      # Si es para pantalla, usar renderizado temporal
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = input$width, height = input$height, res = input$res)
      pheatmap(matriz,
               color = colores,
               clustering_distance_rows = "euclidean",
               clustering_distance_cols = "euclidean",
               clustering_method = "complete",
               main = input$titulo)
      dev.off()
      outfile
    }
  }
  
  # Generar imagen reactiva cuando se pulsa "procesar"
  observeEvent(input$procesar, {
    output$heatmap_zoom <- renderImage({
      list(src = generar_heatmap(), contentType = "image/png", width = "100%")
    }, deleteFile = TRUE)
  })
  
  # Botones de descarga
  output$descargar_png <- downloadHandler(
    filename = function() { "cluster_heatmap.png" },
    content = function(file) { generar_heatmap(file, "png") }
  )
  
  output$descargar_pdf <- downloadHandler(
    filename = function() { "cluster_heatmap.pdf" },
    content = function(file) { generar_heatmap(file, "pdf") }
  )
  
  output$descargar_csv <- downloadHandler(
    filename = function() { "datos_filtrados.csv" },
    content = function(file) {
      write.csv(datos(), file)
    }
  )
}

#EJECUTAR LA APP

shinyApp(ui = ui, server = server)
