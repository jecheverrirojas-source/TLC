# R/data_loader.R - Funciones para carga de datos

#' Cargar datos de acciones desde Yahoo Finance
#'
#' @param ticker Símbolo de la acción
#' @param from Fecha de inicio
#' @param to Fecha de fin
#' @return Lista con precios, retornos y metadatos
load_stock_data <- function(ticker, from, to) {
  
  # Validar entrada
  if (is.null(ticker) || ticker == "") {
    stop("El ticker no puede estar vacío")
  }
  
  ticker <- toupper(ticker)
  from <- as.character(from)
  to <- as.character(to)
  
  cat("Descargando datos para", ticker, "...\n")
  
  # Crear entorno para descarga
  env <- new.env()
  
  # Descargar datos con manejo de errores
  tryCatch({
    getSymbols(Symbols = ticker, 
               src = "yahoo", 
               from = from, 
               to = to,
               env = env, 
               auto.assign = TRUE, 
               warnings = FALSE, 
               quiet = TRUE)
  }, error = function(e) {
    stop(paste("Error al descargar datos para", ticker, ":", e$message))
  })
  
  # Obtener precios de cierre
  if (!exists(ticker, envir = env)) {
    stop(paste("No se encontraron datos para", ticker))
  }
  
  xt <- get(ticker, envir = env)
  prices <- as.numeric(Cl(xt))
  dates <- index(xt)
  
  # Calcular retornos
  returns <- calculate_returns(prices)
  
  # Información del activo
  asset_info <- list(
    symbol = ticker,
    start_date = as.character(min(dates)),
    end_date = as.character(max(dates)),
    n_prices = length(prices),
    n_returns = length(returns)
  )
  
  cat("✓ Datos cargados:", asset_info$n_prices, "precios,", 
      asset_info$n_returns, "retornos\n")
  
  return(list(
    prices = prices,
    returns = returns,
    dates = dates,
    name = ticker,
    info = asset_info
  ))
}

#' Validar ticker de Yahoo Finance
#'
#' @param ticker Símbolo a validar
#' @return TRUE si el ticker es válido
validate_ticker <- function(ticker) {
  if (!is.character(ticker) || length(ticker) != 1) {
    return(FALSE)
  }
  
  # Expresión regular para tickers válidos (puedes ajustarla)
  grepl("^[A-Z0-9.-]{1,10}$", ticker)
}