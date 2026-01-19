# Shiny TCL Finance

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-1E6BA9?style=for-the-badge&logo=r&logoColor=white)
![License](https://img.shields.io/badge/License-MIT-blue.svg)

Una aplicación Shiny interactiva que demuestra conceptos estadísticos fundamentales aplicados a datos financieros: Ley de los Grandes Números, Teorema del Límite Central e Inferencia Estadística.

## 🌟 Características

### 📊 Visualización Interactiva
- **Ley de los Grandes Números**: Distribución de medias muestrales para diferentes tamaños de muestra
- **Teorema del Límite Central**: Convergencia a distribución normal estandarizada
- **Histogramas dinámicos**: Distribución de precios con regla de Sturges

### 🔬 Inferencia Estadística Avanzada
- **Intervalos de confianza**: Asintóticos usando distribución normal
- **Tests de hipótesis**: Test t para retorno medio nulo (H₀: μ = 0)
- **Análisis móvil**: Test t en ventanas temporales dinámicas

### 🔄 Bootstrap
- **Remuestreo no paramétrico**: Distribución empírica de estimadores
- **Comparación de métodos**: Bootstrap vs métodos asintóticos
- **Validación de supuestos**: Evaluación de robustez estadística

### 📈 Datos en Tiempo Real
- **Conexión con Yahoo Finance**: Datos actualizados de mercados financieros
- **Múltiples activos**: Soporte para cualquier ticker disponible
- **Series temporales**: Análisis de retornos logarítmicos

## 🚀 Instalación

### Requisitos Previos
- R (versión 4.0 o superior)
- RStudio (recomendado)

### Instalación Rápida

```r
# Clonar repositorio
git clone https://github.com/tuusuario/shiny-tcl-finance.git
cd shiny-tcl-finance

# Instalar dependencias
source("requirements.R")

# Ejecutar aplicación
shiny::runApp()