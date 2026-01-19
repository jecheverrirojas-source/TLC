# R/returns_processing.R - Procesamiento de retornos

#' Calcular retornos logarítmicos
#'
#' @param prices Vector de precios
#' @return Vector de retornos logarítmicos
calculate_returns <- function(prices) {
  returns <- diff(log(prices))
  returns <- returns[!is.na(returns) & is.finite(returns)]
  
  if (length(returns) == 0) {
    warning("No se pudieron calcular retornos válidos")
    return(numeric(0))
  }
  
  return(returns)
}

#' Calcular estadísticas descriptivas de retornos
#'
#' @param returns Vector de retornos
#' @return Data frame con estadísticas
calculate_return_stats <- function(returns) {
  if (length(returns) == 0) {
    return(data.frame())
  }
  
  data.frame(
    Media = mean(returns),
    Desviacion = sd(returns),
    Minimo = min(returns),
    Maximo = max(returns),
    Sesgo = moments::skewness(returns),
    Curtosis = moments::kurtosis(returns),
    Observaciones = length(returns)
  )
}

#' Detectar outliers en retornos
#'
#' @param returns Vector de retornos
#' @param threshold Umbral para outlier (en desviaciones estándar)
#' @return Lista con outliers e índices
detect_outliers <- function(returns, threshold = 3) {
  z_scores <- scale(returns)
  outliers_idx <- which(abs(z_scores) > threshold)
  
  list(
    indices = outliers_idx,
    values = returns[outliers_idx],
    n_outliers = length(outliers_idx),
    percentage = length(outliers_idx) / length(returns) * 100
  )
}