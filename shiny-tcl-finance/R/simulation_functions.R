# R/simulation_functions.R - Funciones de simulación

#' Simular medias muestrales para diferentes tamaños de muestra
#'
#' @param prices Vector de precios
#' @param tamaños Vector de tamaños de muestra
#' @param replicates Número de réplicas
#' @return Data frame con medias simuladas
simulate_means <- function(prices, tamaños, replicates = 1000) {
  if (length(prices) < max(tamaños)) {
    warning("Algunos tamaños de muestra son mayores que el número de precios disponibles")
    tamaños <- tamaños[tamaños <= length(prices)]
  }
  
  df <- data.frame()
  for (n in tamaños) {
    medias <- replicate(replicates, mean(sample(prices, n, replace = TRUE)))
    df <- rbind(df, data.frame(media = medias, n = n))
  }
  df
}

#' Realizar simulación completa
#'
#' @param prices Vector de precios
#' @param tamaños Vector de tamaños de muestra
#' @param replicates Número de réplicas
#' @param name Nombre del activo
#' @return Lista con resultados de simulación
perform_simulation <- function(prices, tamaños, replicates, name) {
  cat("Realizando simulación para", name, "...\n")
  
  df_res <- simulate_means(prices, tamaños, replicates)
  
  resultados <- list(
    df_resultados = df_res,
    precios = prices,
    media_global = mean(prices, na.rm = TRUE),
    sd_global = sd(prices, na.rm = TRUE),
    tamaños = tamaños,
    name = name,
    replicates = replicates
  )
  
  cat("✓ Simulación completada:", nrow(df_res), "observaciones\n")
  
  return(resultados)
}