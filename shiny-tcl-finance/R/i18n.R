# R/i18n.R - Sistema de internacionalización

#' Configuración de idiomas soportados
i18n_config <- list(
  languages = c("es", "en", "pt"),
  default_language = "es",
  translations = list()
)

#' Cargar traducciones
load_translations <- function() {
  i18n_config$translations <- list(
    
    # Español
    es = list(
      # Títulos
      app_title = "Explorador LGN & TLC Interactivo con Inferencia Estadística",
      tab_prices = "Distribución de Precios",
      tab_lgn = "Ley Grandes Números", 
      tab_tlc = "Teorema Límite Central",
      tab_inference = "Inferencia Formal",
      tab_bootstrap = "Bootstrap",
      tab_info = "Información",
      
      # Panel lateral
      data_config = "Configuración de Datos",
      ticker_label = "Ticker (Yahoo Finance):",
      date_range = "Rango de Fechas:",
      sim_config = "Configuración de Simulación",
      replicates_label = "Réplicas por n:",
      sample_sizes = "Tamaños de Muestra (separados por coma):",
      show_ci = "Mostrar IC 95% sombreado",
      inference_config = "Configuración de Inferencia",
      conf_level_label = "Nivel de Confianza (%):",
      window_size_label = "Tamaño Ventana Móvil (días):",
      bootstrap_reps_label = "Réplicas Bootstrap:",
      run_button = "Ejecutar Simulación",
      
      # Mensajes
      loading_data = "Cargando datos...",
      simulation_running = "Ejecutando simulación...",
      bootstrap_running = "Realizando bootstrap...",
      complete = "Completado",
      error = "Error",
      warning = "Advertencia",
      
      # Inferencia
      ci_title = "Intervalos de Confianza",
      t_test_title = "Test t: H₀: μ = 0",
      t_test_results = "RESULTADOS TEST t PARA UNA MUESTRA",
      hypothesis_null = "Hipótesis nula H₀: μ = 0 (retorno medio nulo)",
      hypothesis_alt = "Hipótesis alternativa H₁: μ ≠ 0",
      t_statistic = "Estadístico t:",
      df = "Grados de libertad:",
      p_value = "Valor-p:",
      sample_mean = "Media muestral:",
      ci_label = "IC",
      interpretation = "INTERPRETACIÓN:",
      evidence_strong = "Evidence against H0 is STRONG (p < 0.01)",
      evidence_moderate = "Evidence against H0 is MODERATE (p < 0.05)",
      evidence_weak = "Evidence against H0 is WEAK (p < 0.10)",
      evidence_very_weak = "Evidence against H0 is VERY WEAK (p ≥ 0.10)",
      reject_h0 = "Se rechaza H₀: El retorno medio es significativamente diferente de cero.",
      weak_evidence = "Evidencia débil contra H₀, pero no suficiente para rechazar.",
      insufficient_evidence = "No hay evidencia suficiente para rechazar H₀.",
      
      # Bootstrap
      bootstrap_title = "Distribución Bootstrap",
      bootstrap_mean = "Bootstrap de la Media del Retorno",
      methods_comparison = "Comparación de Métodos",
      bootstrap_percentile = "Bootstrap Percentil",
      asymptotic = "Asintótico",
      lower_bound = "L.Inferior",
      upper_bound = "L.Superior",
      std_error = "Error.Est",
      comparison_title = "COMPARACIÓN MÉTODOS DE INFERENCIA",
      differences = "DIFERENCIAS EN LÍMITES DE CONFIANZA:",
      lower_diff = "Límite inferior:",
      upper_diff = "Límite superior:",
      interpretation_comparison = "INTERPRETACIÓN:",
      methods_equivalent = "Los métodos son esencialmente equivalentes.",
      small_differences = "Pequeñas diferencias entre métodos.",
      notable_differences = "Diferencias apreciables entre métodos.",
      recommendation = "RECOMENDACIÓN:",
      recommendation_n30 = "Con n < 30, preferir Bootstrap (menos dependiente de normalidad).",
      recommendation_n100 = "Con n moderado, ambos métodos son razonables.",
      recommendation_nlarge = "Con n grande, ambos métodos convergen (TCL aplica).",
      bootstrap_error = "No se pudo calcular el intervalo de confianza bootstrap.",
      possible_reasons = "Posiblemente hay muy pocos datos o el bootstrap falló.",
      
      # Gráficos
      histogram_title = "Histograma (Sturges)",
      price_label = "Precio",
      frequency_label = "Frecuencia",
      lgn_title = "Ley de los Grandes Números - IC 95%",
      sample_mean_label = "Media muestral",
      density_label = "Densidad",
      tlc_title = "TLC KDE (estandarizado)",
      rolling_test_title = "Test t en ventanas móviles",
      bootstrap_dist_title = "Distribución Bootstrap de la Media",
      bootstrap_mean_label = "Media Bootstrap",
      comparison_plot_title = "Comparación IC: Asintótico vs Bootstrap",
      return_mean_label = "Retorno Medio",
      method_label = "Método"
    ),
    
    # English
    en = list(
      # Titles
      app_title = "Interactive LGN & CLT Explorer with Statistical Inference",
      tab_prices = "Price Distribution",
      tab_lgn = "Law of Large Numbers", 
      tab_tlc = "Central Limit Theorem",
      tab_inference = "Formal Inference",
      tab_bootstrap = "Bootstrap",
      tab_info = "Information",
      
      # Sidebar
      data_config = "Data Configuration",
      ticker_label = "Ticker (Yahoo Finance):",
      date_range = "Date Range:",
      sim_config = "Simulation Configuration",
      replicates_label = "Replicates per n:",
      sample_sizes = "Sample Sizes (comma separated):",
      show_ci = "Shade 95% CI",
      inference_config = "Inference Configuration",
      conf_level_label = "Confidence Level (%):",
      window_size_label = "Moving Window Size (days):",
      bootstrap_reps_label = "Bootstrap Replicates:",
      run_button = "Run Simulation",
      
      # Messages
      loading_data = "Loading data...",
      simulation_running = "Running simulation...",
      bootstrap_running = "Performing bootstrap...",
      complete = "Complete",
      error = "Error",
      warning = "Warning",
      
      # Inference
      ci_title = "Confidence Intervals",
      t_test_title = "t-test: H₀: μ = 0",
      t_test_results = "ONE-SAMPLE T-TEST RESULTS",
      hypothesis_null = "Null hypothesis H₀: μ = 0 (zero mean return)",
      hypothesis_alt = "Alternative hypothesis H₁: μ ≠ 0",
      t_statistic = "t statistic:",
      df = "Degrees of freedom:",
      p_value = "p-value:",
      sample_mean = "Sample mean:",
      ci_label = "CI",
      interpretation = "INTERPRETATION:",
      evidence_strong = "Evidence against H0 is STRONG (p < 0.01)",
      evidence_moderate = "Evidence against H0 is MODERATE (p < 0.05)",
      evidence_weak = "Evidence against H0 is WEAK (p < 0.10)",
      evidence_very_weak = "Evidence against H0 is VERY WEAK (p ≥ 0.10)",
      reject_h0 = "Reject H₀: The mean return is significantly different from zero.",
      weak_evidence = "Weak evidence against H₀, but insufficient to reject.",
      insufficient_evidence = "Insufficient evidence to reject H₀.",
      
      # Bootstrap
      bootstrap_title = "Bootstrap Distribution",
      bootstrap_mean = "Return Mean Bootstrap",
      methods_comparison = "Methods Comparison",
      bootstrap_percentile = "Bootstrap Percentile",
      asymptotic = "Asymptotic",
      lower_bound = "Lower.Bound",
      upper_bound = "Upper.Bound",
      std_error = "Std.Error",
      comparison_title = "INFERENCE METHODS COMPARISON",
      differences = "CONFIDENCE LIMITS DIFFERENCES:",
      lower_diff = "Lower limit:",
      upper_diff = "Upper limit:",
      interpretation_comparison = "INTERPRETATION:",
      methods_equivalent = "Methods are essentially equivalent.",
      small_differences = "Small differences between methods.",
      notable_differences = "Notable differences between methods.",
      recommendation = "RECOMMENDATION:",
      recommendation_n30 = "With n < 30, prefer Bootstrap (less dependent on normality).",
      recommendation_n100 = "With moderate n, both methods are reasonable.",
      recommendation_nlarge = "With large n, both methods converge (CLT applies).",
      bootstrap_error = "Could not calculate bootstrap confidence interval.",
      possible_reasons = "Possibly too little data or bootstrap failed.",
      
      # Plots
      histogram_title = "Histogram (Sturges)",
      price_label = "Price",
      frequency_label = "Frequency",
      lgn_title = "Law of Large Numbers - 95% CI",
      sample_mean_label = "Sample Mean",
      density_label = "Density",
      tlc_title = "CLT KDE (standardized)",
      rolling_test_title = "Moving Window t-test",
      bootstrap_dist_title = "Bootstrap Distribution of the Mean",
      bootstrap_mean_label = "Bootstrap Mean",
      comparison_plot_title = "CI Comparison: Asymptotic vs Bootstrap",
      return_mean_label = "Return Mean",
      method_label = "Method"
    ),
    
    # Português
    pt = list(
      # Títulos
      app_title = "Explorador Interativo LGN & TLC com Inferência Estatística",
      tab_prices = "Distribuição de Preços",
      tab_lgn = "Lei dos Grandes Números", 
      tab_tlc = "Teorema do Limite Central",
      tab_inference = "Inferência Formal",
      tab_bootstrap = "Bootstrap",
      tab_info = "Informação",
      
      # Painel lateral
      data_config = "Configuração de Dados",
      ticker_label = "Ticker (Yahoo Finance):",
      date_range = "Intervalo de Datas:",
      sim_config = "Configuração de Simulação",
      replicates_label = "Réplicas por n:",
      sample_sizes = "Tamanhos de Amostra (separados por vírgula):",
      show_ci = "Mostrar IC 95% sombreado",
      inference_config = "Configuração de Inferência",
      conf_level_label = "Nível de Confiança (%):",
      window_size_label = "Tamanho da Janela Móvel (dias):",
      bootstrap_reps_label = "Réplicas Bootstrap:",
      run_button = "Executar Simulação",
      
      # Mensagens
      loading_data = "Carregando dados...",
      simulation_running = "Executando simulação...",
      bootstrap_running = "Realizando bootstrap...",
      complete = "Completo",
      error = "Erro",
      warning = "Aviso",
      
      # Inferência
      ci_title = "Intervalos de Confiança",
      t_test_title = "Teste t: H₀: μ = 0",
      t_test_results = "RESULTADOS DO TESTE t PARA UMA AMOSTRA",
      hypothesis_null = "Hipótese nula H₀: μ = 0 (retorno médio nulo)",
      hypothesis_alt = "Hipótese alternativa H₁: μ ≠ 0",
      t_statistic = "Estatística t:",
      df = "Graus de liberdade:",
      p_value = "Valor-p:",
      sample_mean = "Média amostral:",
      ci_label = "IC",
      interpretation = "INTERPRETAÇÃO:",
      evidence_strong = "Evidência contra H0 é FORTE (p < 0.01)",
      evidence_moderate = "Evidência contra H0 é MODERADA (p < 0.05)",
      evidence_weak = "Evidência contra H0 é FRACA (p < 0.10)",
      evidence_very_weak = "Evidência contra H0 é MUITO FRACA (p ≥ 0.10)",
      reject_h0 = "Rejeita-se H₀: O retorno médio é significativamente diferente de zero.",
      weak_evidence = "Evidência fraca contra H₀, mas insuficiente para rejeitar.",
      insufficient_evidence = "Evidência insuficiente para rejeitar H₀.",
      
      # Bootstrap
      bootstrap_title = "Distribuição Bootstrap",
      bootstrap_mean = "Bootstrap da Média do Retorno",
      methods_comparison = "Comparação de Métodos",
      bootstrap_percentile = "Bootstrap Percentil",
      asymptotic = "Assintótico",
      lower_bound = "Lim.Inferior",
      upper_bound = "Lim.Superior",
      std_error = "Erro.Pad",
      comparison_title = "COMPARAÇÃO DE MÉTODOS DE INFERÊNCIA",
      differences = "DIFERENÇAS NOS LIMITES DE CONFIANÇA:",
      lower_diff = "Limite inferior:",
      upper_diff = "Limite superior:",
      interpretation_comparison = "INTERPRETAÇÃO:",
      methods_equivalent = "Os métodos são essencialmente equivalentes.",
      small_differences = "Pequenas diferenças entre métodos.",
      notable_differences = "Diferenças notáveis entre métodos.",
      recommendation = "RECOMENDAÇÃO:",
      recommendation_n30 = "Com n < 30, preferir Bootstrap (menos dependente da normalidade).",
      recommendation_n100 = "Com n moderado, ambos os métodos são razoáveis.",
      recommendation_nlarge = "Com n grande, ambos os métodos convergem (TLC aplica-se).",
      bootstrap_error = "Não foi possível calcular o intervalo de confiança bootstrap.",
      possible_reasons = "Possivelmente há poucos dados ou o bootstrap falhou.",
      
      # Gráficos
      histogram_title = "Histograma (Sturges)",
      price_label = "Preço",
      frequency_label = "Frequência",
      lgn_title = "Lei dos Grandes Números - IC 95%",
      sample_mean_label = "Média Amostral",
      density_label = "Densidade",
      tlc_title = "TLC KDE (padronizado)",
      rolling_test_title = "Teste t em janelas móveis",
      bootstrap_dist_title = "Distribuição Bootstrap da Média",
      bootstrap_mean_label = "Média Bootstrap",
      comparison_plot_title = "Comparação IC: Assintótico vs Bootstrap",
      return_mean_label = "Média do Retorno",
      method_label = "Método"
    )
  )
}

#' Obtener traducción
#'
#' @param key Llave de traducción
#' @param lang Idioma ("es", "en", "pt")
#' @return Texto traducido
t <- function(key, lang = NULL) {
  if (is.null(lang)) {
    lang <- i18n_config$default_language
  }
  
  if (!lang %in% names(i18n_config$translations)) {
    lang <- i18n_config$default_language
  }
  
  translation <- i18n_config$translations[[lang]][[key]]
  
  if (is.null(translation)) {
    # Si no hay traducción, devolver la llave o texto en idioma por defecto
    translation <- i18n_config$translations[[i18n_config$default_language]][[key]]
    if (is.null(translation)) {
      return(paste0("[", key, "]"))
    }
  }
  
  return(translation)
}

#' Cambiar idioma
#'
#' @param lang Nuevo idioma
#' @return TRUE si el cambio fue exitoso
set_language <- function(lang) {
  if (lang %in% i18n_config$languages) {
    i18n_config$default_language <- lang
    return(TRUE)
  }
  return(FALSE)
}

#' Obtener idioma actual
#'
#' @return Código de idioma actual
get_current_language <- function() {
  return(i18n_config$default_language)
}

#' Obtener lista de idiomas disponibles
#'
#' @return Lista de idiomas
get_available_languages <- function() {
  return(i18n_config$languages)
}

# Inicializar traducciones al cargar el módulo
load_translations()