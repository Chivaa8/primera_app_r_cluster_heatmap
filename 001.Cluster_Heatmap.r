# ============================================
#  CLUSTER HEATMAP INTERACTIVO
#  Autor: Oriol Chiva Hidalgo
#  Descripción: Visualización avanzada de matrices
#  de expresión génica mediante clustering jerárquico.
# ============================================

library(shiny)
library(pheatmap)
library(DT)
library(ggplot2)
library(bslib)
library(shinycssloaders)

# -----------------------------
# UI (interfaz de usuario)
# -----------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    base_font = font_google("Roboto"),
    heading_font = font_google("Lato"),
    bg = "#0f1116",
    fg = "#f0f0f0",
    primary = "#4DB6AC"
    
  ),
  
  titlePanel("🧬 Cluster Heatmap Interactivo"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "📂 Sube tu matriz de expresión (.csv)", accept = ".csv"),
      
      checkboxInput("escalar", "Escalar filas (Z-score)", value = TRUE),
      
      selectInput("paleta", "🎨 Paleta de colores:",
                  choices = c("Verde-Rojo" = "greenred",
                              "Azul-Blanco-Rojo" = "bwr",
                              "Azul-Amarillo-Rojo" = "ayr")),
      
      selectInput("clust_metodo", "Método de clustering:",
                  choices = c("complete", "ward.D2", "average", "single")),
      
      selectInput("distancia", "Métrica de distancia:",
                  choices = c("euclidean", "correlation", "maximum", "manhattan")),
      
      textInput("titulo", "🧠 Título del Heatmap:", 
                value = "Cluster Heatmap de expresión génica"),
      
      sliderInput("width", "Ancho (PX):", min = 600, max = 4000, value = 1000, step = 100),
      sliderInput("height", "Alto (PX):", min = 600, max = 4000, value = 1000, step = 100),
      sliderInput("res", "Resolución (DPI):", min = 72, max = 600, value = 200, step = 10),
      
      hr(),
      downloadButton("descargar_png", "💾 Descargar PNG"),
      downloadButton("descargar_pdf", "📑 Descargar PDF"),
      downloadButton("descargar_csv", "🧾 Descargar CSV"),
      br(), br(),
      actionButton("procesar", "⚡ Generar Heatmap", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vista de datos",
                 br(),
                 withSpinner(DTOutput("tabla"), color = "#4DB6AC")),
        
        tabPanel("Heatmap interactivo",
                 br(),
                 withSpinner(imageOutput("heatmap_zoom", height = "auto", width = "100%"), color = "#4DB6AC")),
        
        tabPanel("Acerca de",
                 br(),
                 HTML("<h4>🧬 Acerca de esta app</h4>
                      <p>Esta aplicación permite explorar y visualizar matrices de expresión génica 
                      usando clustering jerárquico. Es ideal para análisis de transcriptómica, proteómica
                      o cualquier tipo de datos omicos tabulares.</p>
                      <p><b>Autor:</b> Oriol Chiva Hidalgo<br>
                      <b>Versión:</b> 3.0<br>
                      <b>Licencia:</b> MIT</p>"))
      )
    )
  )
)

# -----------------------------
# SERVER (lógica de backend)
# -----------------------------
server <- function(input, output) {
  
  datos <- reactive({
    req(input$archivo)
    read.csv(input$archivo$datapath, row.names = 1)
  })
  
  output$tabla <- renderDT({
    datatable(
      datos(),
      extensions = "Buttons",
      options = list(
        dom = '<"top"lfB>rt<"bottom"ip>',  # l=length, f=filter, B=buttons, t=table, i=info, p=paging
        buttons = list(
          list(extend = "copy", text = "📋 Copiar")
        ),
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")),
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE,
        autoWidth = TRUE,
        language = list(
          lengthMenu = "Mostrar _MENU_ filas por página",
          search = "Buscar:",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          paginate = list(previous = "Anterior", `next` = "Siguiente"),  # <-- `next` entre backticks o comillas
          buttons = list(copyTitle = "Copiado al portapapeles")
        )
      ),
      filter = "top",
      rownames = TRUE
    )
  })
  
  generar_heatmap <- function(file = NULL, format = "screen",
                              width_px = 1000, height_px = 1000, res_dpi = 200) {
    matriz <- as.matrix(datos())
    if (input$escalar) matriz <- t(scale(t(matriz)))
    
    colores <- switch(input$paleta,
                      "greenred" = colorRampPalette(c("green", "black", "red"))(100),
                      "bwr" = colorRampPalette(c("blue", "white", "red"))(100),
                      "ayr" = colorRampPalette(c("blue", "yellow", "red"))(100))
    
    if (!is.null(file)) {
      if (format == "pdf") {
        pdf(file, width = width_px / res_dpi, height = height_px / res_dpi)
      } else if (format == "png") {
        png(file, width = width_px, height = height_px, units = "px", res = res_dpi)
      }
      pheatmap(matriz,
               color = colores,
               clustering_distance_rows = input$distancia,
               clustering_distance_cols = input$distancia,
               clustering_method = input$clust_metodo,
               main = input$titulo)
      dev.off()
    } else {
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = width_px, height = height_px, units = "px", res = res_dpi)
      pheatmap(matriz,
               color = colores,
               clustering_distance_rows = input$distancia,
               clustering_distance_cols = input$distancia,
               clustering_method = input$clust_metodo,
               main = input$titulo)
      dev.off()
      outfile
    }
  }
  
  observeEvent(input$procesar, {
    output$heatmap_zoom <- renderImage({
      list(src = generar_heatmap(width_px = input$width,
                                 height_px = input$height,
                                 res_dpi = input$res),
           contentType = "image/png",
           width = "100%")
    }, deleteFile = TRUE)
  })
  
  output$descargar_png <- downloadHandler(
    filename = function() { "cluster_heatmap.png" },
    content = function(file) { generar_heatmap(file, "png", input$width, input$height, input$res) }
  )
  
  output$descargar_pdf <- downloadHandler(
    filename = function() { "cluster_heatmap.pdf" },
    content = function(file) { generar_heatmap(file, "pdf", input$width, input$height, input$res) }
  )
  
  output$descargar_csv <- downloadHandler(
    filename = function() { "datos.csv" },
    content = function(file) { write.csv(datos(), file) }
  )
}

# -----------------------------
# Ejecutar la app
# -----------------------------
shinyApp(ui = ui, server = server)
