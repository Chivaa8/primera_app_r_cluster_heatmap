# ============================================
#  CLUSTER HEATMAP INTERACTIVO
# ============================================

library(shiny)
library(pheatmap)
library(DT)
library(ggplot2)
library(bslib)
library(shinycssloaders)
library(viridis)

# -----------------------------
# UI
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
  
  titlePanel("🧬 Cluster Heatmap"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("archivo", "📂 Sube tu matriz de expresión (.csv)", accept = ".csv"),
      
      checkboxInput("escalar", "Escalar filas (Z-score)", value = TRUE),
    
      hr(),  
      
      uiOutput("selector_genes"),
      
      sliderInput("rango_genes", "🔍 Rango de genes por fila (posición):",
                  min = 1, max = 100, value = c(1, 10), step = 1),
      
      hr(),
      
      selectInput("paleta", "🎨 Paleta de colores (accesible):",
                  choices = c("Viridis" = "viridis",
                              "Plasma" = "plasma",
                              "Inferno" = "inferno",
                              "Magma" = "magma",
                              "Cividis (daltonismo)" = "cividis")),
      
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
                 withSpinner(DTOutput("tabla"), color = "#4DB6AC")
        ),
        
        tabPanel("Heatmap interactivo",
                 br(),
                 withSpinner(imageOutput("heatmap_zoom", height = "auto", width = "100%"), color = "#4DB6AC")
        ),
        
        tabPanel("Acerca de",
                 br(),
                 HTML("<h4>🧬 Acerca de esta app</h4>
                      <p>Esta aplicación permite explorar y visualizar matrices de expresión génica 
                      usando clustering jerárquico. Es ideal para análisis de transcriptómica, proteómica
                      o cualquier tipo de datos ómicos tabulares.</p>
                      <p><b>Autor original:</b> Oriol Chiva Hidalgo<br>
                      <b>Versión modificada:</b> 3.2<br>
                      <b>Licencia:</b> MIT</p>")
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$archivo)  
    read.csv(input$archivo$datapath, row.names = 1)  
  })
  
  observe({
    req(datos())
    updateSliderInput(inputId = "rango_genes",
                      min = 1,
                      max = nrow(datos()),
                      value = c(1, min(10, nrow(datos()))))
  })
  
  output$selector_genes <- renderUI({
    req(datos())
    selectInput("genes_seleccionados", "🔬 Selecciona genes:",
                choices = rownames(datos()),
                selected = NULL,
                multiple = TRUE)
  })
  
  output$tabla <- renderDT({
    
    df <- datos()
    
    num_cols <- sapply(df, is.numeric)
    df[, num_cols] <- round(df[, num_cols], 3)  
    
    datatable(
      df,
      extensions = "Buttons",  
      options = list(
        dom = '<"top"lfB>rt<"bottom"ip>',  
        buttons = list(
          list(extend = "copy", text = "📋 Copiar")
        ),
        pageLength = 10, 
        lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")),
        scrollX = TRUE,  
        language = list(
          lengthMenu = "Mostrar _MENU_ filas por página",
          search = "Buscar:",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          paginate = list(previous = "Anterior", `next` = "Siguiente"),
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
    
    if (!is.null(input$genes_seleccionados) && length(input$genes_seleccionados) > 0) {
      matriz <- matriz[rownames(matriz) %in% input$genes_seleccionados, , drop = FALSE]
    } else {
      total_genes <- nrow(matriz)
      desde <- max(1, input$rango_genes[1])
      hasta <- min(total_genes, input$rango_genes[2])
      if (desde <= hasta) {
        matriz <- matriz[desde:hasta, , drop = FALSE]
      }
    }
    
    if (input$escalar) {
      varianzas <- apply(matriz, 1, var)
      matriz <- matriz[varianzas > 0, , drop = FALSE]
      matriz <- t(scale(t(matriz)))
    }
    
    colores <- switch(input$paleta,
                      "viridis" = viridis(100),
                      "plasma" = plasma(100),
                      "inferno" = inferno(100),
                      "magma" = magma(100),
                      "cividis" = cividis(100))
    
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
    content = function(file) {
      generar_heatmap(file, "png", input$width, input$height, input$res)
    }
  )
  
  output$descargar_pdf <- downloadHandler(
    filename = function() { "cluster_heatmap.pdf" },
    content = function(file) {
      generar_heatmap(file, "pdf", input$width, input$height, input$res)
    }
  )
  
  output$descargar_csv <- downloadHandler(
    filename = function() { "datos_filtrados.csv" },
    content = function(file) {
      matriz <- as.matrix(datos())
      
      if (!is.null(input$genes_seleccionados) && length(input$genes_seleccionados) > 0) {
        matriz <- matriz[rownames(matriz) %in% input$genes_seleccionados, , drop = FALSE]
      } else {
        total_genes <- nrow(matriz)
        desde <- max(1, input$rango_genes[1])
        hasta <- min(total_genes, input$rango_genes[2])
        if (desde <= hasta) {
          matriz <- matriz[desde:hasta, , drop = FALSE]
        }
      }
      
      write.csv(matriz, file)
    }
  )
}

# -----------------------------
# Ejecutar Shiny
# -----------------------------
shinyApp(ui = ui, server = server)
