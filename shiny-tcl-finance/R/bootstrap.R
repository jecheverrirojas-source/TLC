# R/bootstrap.R - Funciones de Bootstrap

#' Función para bootstrap de la media
#'
#' @param data Vector de datos
#' @param indices Índices para remuestreo
#' @return Media de la muestra bootstrap
bootstrap_mean <- function(data, indices) {
  return(mean(data[indices]))
}

#' Realizar análisis de bootstrap
#'
#' @param returns Vector de retornos
#' @param R Número de réplicas bootstrap
#' @param conf_level Nivel de confianza
#' @return Lista con resultados bootstrap
perform_bootstrap_analysis <- function(returns, R = 1000, conf_level = 0.95) {
  cat("Realizando bootstrap...\n")
  
  # Bootstrap paramétrico de la media
  boot_result <- boot::boot(returns, bootstrap_mean, R = R)
  
  # Intervalo de confianza bootstrap
  ci_boot <- tryCatch({
    boot::boot.ci(boot_result, type = "perc", conf = conf_level)
  }, error = function(e) {
    warning("Error calculando intervalo de confianza bootstrap: ", e$message)
    list(percent = c(NA, NA, NA, NA, NA, NA))
  })
  
  resultados <- list(
    boot_result = boot_result,
    ci_boot = ci_boot,
    boot_means = boot_result$t,
    R = R,
    conf_level = conf_level
  )
  
  cat("✓ Bootstrap completado:", R, "réplicas\n")
  
  return(resultados)
}

#' Crear resumen de resultados bootstrap
#'
#' @param boot_res Resultados de bootstrap
#' @param inf_res Resultados de inferencia
#' @return Data frame comparativo
create_bootstrap_summary <- function(boot_res, inf_res) {
  ci_boot <- boot_res$ci_boot$percent
  boot_means <- boot_res$boot_means
  
  if(!is.null(ci_boot) && length(ci_boot) >= 5) {
    boot_ci_lower <- ci_boot[4]
    boot_ci_upper <- ci_boot[5]
  } else {
    boot_ci_lower <- NA
    boot_ci_upper <- NA
  }
  
  data.frame(
    Método = c("Bootstrap Percentil", "Asintótico"),
    Media = c(mean(boot_means), inf_res$media_ret),
    L.Inferior = c(boot_ci_lower, inf_res$ci_asymptotic[1]),
    L.Superior = c(boot_ci_upper, inf_res$ci_asymptotic[2]),
    Error.Est = c(sd(boot_means), inf_res$sd_ret/sqrt(inf_res$n)),
    check.names = FALSE
  )
}

#' Imprimir análisis comparativo
#'
#' @param inf_res Resultados de inferencia
#' @param boot_res Resultados de bootstrap
print_comparison_analysis <- function(inf_res, boot_res) {
  if(is.null(inf_res) || is.null(boot_res)) {
    cat("Esperando datos...")
    return()
  }
  
  ci_boot <- boot_res$ci_boot$percent
  
  if(!is.null(ci_boot) && length(ci_boot) >= 5) {
    diff_lower <- abs(inf_res$ci_asymptotic[1] - ci_boot[4])
    diff_upper <- abs(inf_res$ci_asymptotic[2] - ci_boot[5])
    
    cat("COMPARACIÓN MÉTODOS DE INFERENCIA\n")
    cat("==================================\n")
    cat("\nDIFERENCIAS EN LÍMITES DE CONFIANZA:\n")
    cat(paste("Límite inferior:", round(diff_lower, 6), "\n"))
    cat(paste("Límite superior:", round(diff_upper, 6), "\n"))
    
    cat("\nINTERPRETACIÓN:\n")
    if (max(diff_lower, diff_upper) < 0.001) {
      cat("Los métodos son esencialmente equivalentes.")
    } else if (max(diff_lower, diff_upper) < 0.01) {
      cat("Pequeñas diferencias entre métodos.")
    } else {
      cat("Diferencias apreciables entre métodos.")
    }
    
    cat("\n\nRECOMENDACIÓN:\n")
    if (inf_res$n < 30) {
      cat("Con n < 30, preferir Bootstrap (menos dependiente de normalidad).")
    } else if (inf_res$n < 100) {
      cat("Con n moderado, ambos métodos son razonables.")
    } else {
      cat("Con n grande, ambos métodos convergen (TCL aplica).")
    }
  } else {
    cat("No se pudo calcular el intervalo de confianza bootstrap.\n")
    cat("Posiblemente hay muy pocos datos o el bootstrap falló.")
  }
}