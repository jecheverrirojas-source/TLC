# R/inference.R - Funciones de inferencia estadística con internacionalización

#' Realizar análisis de inferencia
#'
#' @param returns Vector de retornos
#' @param conf_level Nivel de confianza
#' @param window_size Tamaño de ventana para análisis móvil
#' @return Lista con resultados de inferencia
perform_inference_analysis <- function(returns, conf_level = 0.95, window_size = 30) {
  
  n <- length(returns)
  
  if (n == 0) {
    stop("No hay retornos válidos para analizar")
  }
  
  media_ret <- mean(returns)
  sd_ret <- sd(returns)
  alpha <- 1 - conf_level
  z_critical <- qnorm(1 - alpha/2)
  
  # Intervalo de confianza asintótico
  ci_lower <- media_ret - z_critical * (sd_ret/sqrt(n))
  ci_upper <- media_ret + z_critical * (sd_ret/sqrt(n))
  
  # Test t para H0: μ = 0
  t_test <- t.test(returns, mu = 0, conf.level = conf_level)
  
  # Test t en ventanas móviles
  if (window_size < length(returns)) {
    rolling_results <- lapply(1:(length(returns) - window_size + 1), function(i) {
      window_returns <- returns[i:(i + window_size - 1)]
      test_result <- t.test(window_returns, mu = 0)
      data.frame(
        t_stat = as.numeric(test_result$statistic),
        p_value = test_result$p.value,
        mean_return = mean(window_returns),
        window_start = i,
        window_end = i + window_size - 1
      )
    })
    rolling_t <- do.call(rbind, rolling_results)
  } else {
    rolling_t <- NULL
  }
  
  list(
    returns = returns,
    n = n,
    media_ret = media_ret,
    sd_ret = sd_ret,
    ci_asymptotic = c(ci_lower, ci_upper),
    t_test = t_test,
    rolling_t = rolling_t,
    conf_level = conf_level
  )
}

#' Crear tabla de intervalos de confianza
#'
#' @param inf_res Resultados de inferencia
#' @param conf_level Nivel de confianza (en porcentaje)
#' @param lang Idioma ("es", "en", "pt")
#' @return Data frame con intervalos
create_ci_table <- function(inf_res, conf_level, lang = "es") {
  
  # Cargar función de traducción si no está disponible
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  data.frame(
    Método = t("asymptotic_method", lang),
    Media = round(inf_res$media_ret, 6),
    L.Inferior = round(inf_res$ci_asymptotic[1], 6),
    L.Superior = round(inf_res$ci_asymptotic[2], 6),
    Nivel.Conf = paste0(conf_level, "%"),
    stringsAsFactors = FALSE
  )
}

#' Imprimir resultados del test t con internacionalización
#'
#' @param t_test Resultado de t.test()
#' @param conf_level Nivel de confianza
#' @param lang Idioma ("es", "en", "pt")
print_t_test_results <- function(t_test, conf_level, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  cat(t("t_test_results", lang), "\n")
  cat("===================================\n")
  cat(t("hypothesis_null", lang), "\n")
  cat(t("hypothesis_alt", lang), "\n")
  cat("\n")
  cat(paste(t("t_statistic", lang), round(t_test$statistic, 4), "\n"))
  cat(paste(t("df", lang), t_test$parameter, "\n"))
  cat(paste(t("p_value", lang), format.pval(t_test$p.value, digits = 4), "\n"))
  cat(paste(t("sample_mean", lang), round(t_test$estimate, 6), "\n"))
  cat(paste(t("ci_label", lang), conf_level*100, "%: [", 
            round(t_test$conf.int[1], 6), ", ", 
            round(t_test$conf.int[2], 6), "]\n", sep = ""))
  
  cat("\n", t("interpretation", lang), ":\n", sep = "")
  
  if (t_test$p.value < 0.01) {
    cat(t("evidence_strong", lang), "\n")
    cat(t("reject_h0", lang))
  } else if (t_test$p.value < 0.05) {
    cat(t("evidence_moderate", lang), "\n")
    cat(t("reject_h0", lang))
  } else if (t_test$p.value < 0.10) {
    cat(t("evidence_weak", lang), "\n")
    cat(t("weak_evidence", lang))
  } else {
    cat(t("evidence_very_weak", lang), "\n")
    cat(t("insufficient_evidence", lang))
  }
}

#' Calcular intervalos de confianza para diferentes niveles
#'
#' @param returns Vector de retornos
#' @param levels Niveles de confianza (ej: c(0.90, 0.95, 0.99))
#' @return Data frame con intervalos para cada nivel
calculate_multiple_cis <- function(returns, levels = c(0.90, 0.95, 0.99)) {
  
  n <- length(returns)
  media_ret <- mean(returns)
  sd_ret <- sd(returns)
  
  cis <- lapply(levels, function(conf_level) {
    alpha <- 1 - conf_level
    z_critical <- qnorm(1 - alpha/2)
    margin_error <- z_critical * (sd_ret/sqrt(n))
    
    data.frame(
      Nivel = paste0(conf_level*100, "%"),
      Z_Crítico = round(z_critical, 3),
      Margen_Error = round(margin_error, 6),
      Inferior = round(media_ret - margin_error, 6),
      Superior = round(media_ret + margin_error, 6),
      Amplitud = round(2 * margin_error, 6)
    )
  })
  
  do.call(rbind, cis)
}

#' Realizar prueba de normalidad
#'
#' @param returns Vector de retornos
#' @param tests Tipos de test ("shapiro", "ks", "ad")
#' @return Lista con resultados de pruebas de normalidad
test_normality <- function(returns, tests = c("shapiro", "ks", "ad")) {
  
  results <- list()
  
  if ("shapiro" %in% tests) {
    if (length(returns) <= 5000) {
      results$shapiro <- shapiro.test(returns)
    } else {
      results$shapiro <- list(
        statistic = NA,
        p.value = NA,
        method = "Shapiro-Wilk test not applicable for n > 5000"
      )
    }
  }
  
  if ("ks" %in% tests) {
    ks_test <- ks.test(returns, "pnorm", mean = mean(returns), sd = sd(returns))
    results$kolmogorov_smirnov <- ks_test
  }
  
  if ("ad" %in% tests) {
    if (requireNamespace("nortest", quietly = TRUE)) {
      results$anderson_darling <- nortest::ad.test(returns)
    }
  }
  
  return(results)
}

#' Calcular poder estadístico del test t
#'
#' @param returns Vector de retornos
#' @param effect_size Tamaño del efecto (d de Cohen)
#' @param alpha Nivel de significancia
#' @return Poder estadístico
calculate_power <- function(returns, effect_size = NULL, alpha = 0.05) {
  
  n <- length(returns)
  
  # Si no se especifica effect_size, calcularlo a partir de los datos
  if (is.null(effect_size)) {
    effect_size <- abs(mean(returns)) / sd(returns)
  }
  
  # Usar power.t.test para calcular el poder
  power_test <- power.t.test(
    n = n,
    delta = effect_size,
    sd = sd(returns),
    sig.level = alpha,
    type = "one.sample",
    alternative = "two.sided"
  )
  
  return(power_test$power)
}

#' Realizar análisis de sensibilidad
#'
#' @param returns Vector de retornos
#' @param conf_levels Niveles de confianza a analizar
#' @param sample_sizes Tamaños de muestra a simular
#' @return Lista con análisis de sensibilidad
sensitivity_analysis <- function(returns, 
                                 conf_levels = seq(0.80, 0.99, 0.01),
                                 sample_sizes = c(30, 50, 100, 200, 500)) {
  
  media_ret <- mean(returns)
  sd_ret <- sd(returns)
  n_original <- length(returns)
  
  # Sensibilidad a nivel de confianza
  sens_conf <- sapply(conf_levels, function(cl) {
    z <- qnorm(1 - (1 - cl)/2)
    margin <- z * (sd_ret/sqrt(n_original))
    data.frame(
      conf_level = cl,
      margin_error = margin,
      width = 2 * margin
    )
  }, simplify = FALSE)
  sens_conf <- do.call(rbind, sens_conf)
  
  # Sensibilidad a tamaño de muestra
  sens_sample <- sapply(sample_sizes, function(n) {
    z <- qnorm(1 - (1 - 0.95)/2)  # 95% confianza
    margin <- z * (sd_ret/sqrt(n))
    data.frame(
      sample_size = n,
      margin_error = margin,
      width = 2 * margin,
      relative_precision = (sd_ret/sqrt(n_original)) / (sd_ret/sqrt(n))
    )
  }, simplify = FALSE)
  sens_sample <- do.call(rbind, sens_sample)
  
  list(
    confidence_sensitivity = sens_conf,
    sample_size_sensitivity = sens_sample,
    original_sample_size = n_original,
    original_margin_error = qnorm(0.975) * (sd_ret/sqrt(n_original))
  )
}

#' Crear reporte completo de inferencia
#'
#' @param inf_res Resultados de inferencia
#' @param lang Idioma ("es", "en", "pt")
#' @return Lista con reporte completo
create_inference_report <- function(inf_res, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  # Prueba de normalidad
  normality_tests <- test_normality(inf_res$returns)
  
  # Poder estadístico
  power <- calculate_power(inf_res$returns)
  
  # Análisis de sensibilidad
  sensitivity <- sensitivity_analysis(inf_res$returns)
  
  # Múltiples intervalos de confianza
  multiple_cis <- calculate_multiple_cis(inf_res$returns)
  
  list(
    basic_results = inf_res,
    normality_tests = normality_tests,
    statistical_power = power,
    sensitivity_analysis = sensitivity,
    multiple_confidence_intervals = multiple_cis,
    sample_summary = data.frame(
      n = inf_res$n,
      mean = inf_res$media_ret,
      sd = inf_res$sd_ret,
      se = inf_res$sd_ret/sqrt(inf_res$n),
      min = min(inf_res$returns),
      max = max(inf_res$returns),
      skewness = moments::skewness(inf_res$returns),
      kurtosis = moments::kurtosis(inf_res$returns)
    )
  )
}

#' Función helper para formatear números
#'
#' @param x Número a formatear
#' @param digits Dígitos decimales
#' @return String formateado
format_num <- function(x, digits = 4) {
  if (is.na(x)) return("NA")
  format(round(x, digits), nsmall = digits, scientific = FALSE)
}

#' Exportar resultados de inferencia a CSV
#'
#' @param inf_res Resultados de inferencia
#' @param file_path Ruta del archivo
#' @param lang Idioma ("es", "en", "pt")
export_inference_results <- function(inf_res, file_path = "inference_results.csv", lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  # Crear data frame con resultados principales
  results_df <- data.frame(
    Variable = c(
      t("sample_size", lang),
      t("sample_mean", lang),
      t("sample_sd", lang),
      t("standard_error", lang),
      t("t_statistic", lang),
      t("p_value", lang),
      t("ci_lower", lang),
      t("ci_upper", lang)
    ),
    Value = c(
      inf_res$n,
      format_num(inf_res$media_ret, 6),
      format_num(inf_res$sd_ret, 6),
      format_num(inf_res$sd_ret/sqrt(inf_res$n), 6),
      format_num(inf_res$t_test$statistic, 4),
      format_num(inf_res$t_test$p.value, 6),
      format_num(inf_res$ci_asymptotic[1], 6),
      format_num(inf_res$ci_asymptotic[2], 6)
    )
  )
  
  # Escribir a CSV
  write.csv(results_df, file_path, row.names = FALSE)
  
  cat(t("results_exported", lang), ":", file_path, "\n")
}