# global.R - Configuración global y carga de librerías

# Instalar paquetes si es necesario
if (!require("shiny")) install.packages("shiny")
if (!require("quantmod")) install.packages("quantmod")
if (!require("dplyr")) install.packages("dplyr")
if (!require("plotly")) install.packages("plotly")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("purrr")) install.packages("purrr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("broom")) install.packages("broom")
if (!require("boot")) install.packages("boot")

# Cargar librerías
library(shiny)
library(quantmod)
library(dplyr)
library(plotly)
library(ggplot2)
library(purrr)
library(tidyr)
library(broom)
library(boot)

# Configuración global
options(shiny.sanitize.errors = FALSE)
options(scipen = 999)

# Mensaje de inicio
cat("Aplicación Shiny TCL Finance cargada correctamente\n")