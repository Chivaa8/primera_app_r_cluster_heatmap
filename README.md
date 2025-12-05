# 🧬 Shiny App – Cluster Heatmap Interactivo (Bioinformática)

![R Shiny](https://img.shields.io/badge/R-Shiny-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Status](https://img.shields.io/badge/version-2.1-brightgreen)

---

## 📖 Descripción

Aplicación **interactiva desarrollada en R Shiny** para generar **heatmaps con clustering jerárquico** a partir de **matrices de expresión génica** (por ejemplo, RNA-seq, microarrays o proteómica cuantitativa).

Permite explorar datos, ajustar la visualización, cambiar la paleta de colores, personalizar títulos y exportar figuras de alta resolución para publicaciones científicas.

---

## ⚙️ Funcionalidades

- 📂 Carga de archivos `.csv` con matrices de expresión.
- ⚖️ Escalado opcional de genes (Z-score).
- 🎨 Selección de paleta de colores.
- 🧭 Personalización del título del gráfico.
- 📏 Control del tamaño y resolución (px, dpi).
- 💾 Descarga en **PNG**, **PDF** o **CSV**.
- 🔍 Vista previa interactiva con `DT`.

---

## 🧩 Estructura del proyecto
bioinfo-heatmap-app/
│
├── app.R # Código principal de la app
├── example_data.csv # Datos de prueba (matriz de expresión)
└── README.md # Descripción del proyecto


---

## 🚀 Ejecución

### 1️⃣ Instala los paquetes necesarios:

```r
install.packages(c("shiny", "pheatmap", "DT", "ggplot2","shinycssloaders"))

```

### 2️⃣ Ejecuta la app:

```r
shiny::runApp("app.R")

```

### 3️⃣ Sube el archivo de ejemplo (example_data.csv) y pulsa Generar Heatmap.
### 🧠 Datos de ejemplo

Incluye un archivo example_data.csv con 100 genes y 5 muestras simuladas, con 3 grupos de expresión distintos para mostrar patrones de clustering.

### 👤 Autor

Desarrollado por Oriol Chiva Hidalgo
### 📧 Contacto: oriolchiva8@gmail.com / oriol.chiva.hidalgo@gmail.com


© 2025 – Proyecto educativo y de investigación bajo licencia MIT.
