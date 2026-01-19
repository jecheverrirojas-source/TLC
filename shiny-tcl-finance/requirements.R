# requirements.R - Lista de dependencias del proyecto

#' Instalar y cargar todos los paquetes necesarios
#'
#' @return Mensaje de confirmación
setup_environment <- function() {
  packages <- c(
    "shiny",      # Framework para aplicaciones web
    "quantmod",   # Descarga de datos financieros
    "dplyr",      # Manipulación de datos
    "plotly",     # Gráficos interactivos
    "ggplot2",    # Gráficos estáticos
    "purrr",      # Programación funcional
    "tidyr",      # Limpieza de datos
    "broom",      # Tidy resultados estadísticos
    "boot",       # Bootstrap
    "markdown",   # Para README.md
    "knitr"       # Para reportes
  )
  
  cat("=== Configuración del Entorno ===\n")
  
  # Instalar paquetes faltantes
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(!installed_packages)) {
    cat("Instalando paquetes faltantes...\n")
    install.packages(packages[!installed_packages], dependencies = TRUE)
  }
  
  # Cargar paquetes
  cat("Cargando paquetes...\n")
  invisible(lapply(packages, library, character.only = TRUE))
  
  cat("¡Entorno configurado correctamente!\n")
  cat("Paquetes cargados:", paste(packages, collapse = ", "), "\n")
}

# Ejecutar configuración si se corre directamente
if (sys.nframe() == 0) {
  setup_environment()
}