# R/plot_functions.R - Funciones de visualización con internacionalización

#' Graficar histograma de precios
#'
#' @param prices Vector de precios
#' @param name Nombre del activo
#' @param lang Idioma ("es", "en", "pt")
#' @return Objeto plotly
plot_histogram <- function(prices, name, lang = "es") {
  
  # Cargar función de traducción si no está disponible
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  n_bins <- nclass.Sturges(prices)
  media_precio <- mean(prices, na.rm = TRUE)
  x_breaks <- pretty(prices, n = n_bins)
  
  # Textos traducidos
  title <- paste0(t("histogram_title", lang), " - ", name)
  x_label <- t("price_label", lang)
  y_label <- t("frequency_label", lang)
  mean_text <- paste0(t("sample_mean", lang), " = ", round(media_precio, 2))
  
  hist_gg <- ggplot(data.frame(price = prices), aes(x = price)) +
    geom_histogram(bins = n_bins, fill = "lightblue", color = "white", alpha = 0.8) +
    geom_vline(xintercept = media_precio, color = "darkblue", linetype = "dashed", linewidth = 1) +
    annotate("text", x = media_precio, y = Inf, 
             label = mean_text,
             color = "darkblue", angle = 90, vjust = 1.5, hjust = 1.1) +
    scale_x_continuous(breaks = x_breaks) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 14)
  
  ggplotly(hist_gg)
}

#' Graficar KDE para Ley de Grandes Números
#'
#' @param df_resultados Data frame con resultados
#' @param media_global Media poblacional
#' @param sd_global Desviación estándar poblacional
#' @param tamaños Tamaños de muestra
#' @param show_ic_shaded Mostrar IC sombreado
#' @param name Nombre del activo
#' @param lang Idioma ("es", "en", "pt")
#' @return Objeto plotly
plot_kde_lgn <- function(df_resultados, media_global, sd_global, tamaños, 
                         show_ic_shaded = FALSE, name, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  dens_lgn <- df_resultados %>%
    group_by(n) %>%
    do({
      dens <- density(.$media)
      data.frame(x = dens$x, y = dens$y, n = unique(.$n))
    })
  
  norm_lgn <- lapply(tamaños, function(n) {
    x <- seq(min(df_resultados$media), max(df_resultados$media), length.out = 200)
    y <- dnorm(x, mean = media_global, sd = sd_global / sqrt(n))
    data.frame(x = x, y = y, n = n)
  }) %>% bind_rows()
  
  linea_media <- lapply(tamaños, function(n) {
    y_max <- max(dens_lgn$y[dens_lgn$n == n])
    data.frame(x = c(media_global, media_global), y = c(0, y_max), n = n)
  }) %>% bind_rows()
  
  ic95 <- lapply(tamaños, function(n) {
    ic_inf <- media_global - 1.96 * (sd_global / sqrt(n))
    ic_sup <- media_global + 1.96 * (sd_global / sqrt(n))
    y_max <- max(dens_lgn$y[dens_lgn$n == n])
    data.frame(ic_inf = ic_inf, ic_sup = ic_sup, y = y_max, n = n)
  }) %>% bind_rows()
  
  # Textos traducidos
  title <- paste(t("lgn_title", lang), name)
  x_label <- t("sample_mean_label", lang)
  y_label <- t("density_label", lang)
  
  p <- plot_ly() %>%
    add_lines(data = dens_lgn, x = ~x, y = ~y, frame = ~n, 
              color = I("lightgreen"), name = "KDE") %>%
    add_lines(data = norm_lgn, x = ~x, y = ~y, frame = ~n, 
              color = I("black"), name = t("asymptotic", lang)) %>%
    add_lines(data = linea_media, x = ~x, y = ~y, frame = ~n, 
              color = I("darkgreen"), line = list(dash = "dot"), 
              name = t("population_mean", lang))
  
  if(show_ic_shaded){
    poly_df <- ic95 %>% group_by(n) %>% 
      do(data.frame(x = c(.$ic_inf, .$ic_sup, .$ic_sup, .$ic_inf),
                    y = c(0,0,.$y,.$y),
                    n = .$n))
    p <- p %>%
      add_polygons(data = poly_df, x = ~x, y = ~y, frame = ~n,
                   fillcolor = 'rgba(0,100,200,0.15)', line = list(width = 0), 
                   name = "95% CI")
  } else {
    ic_df_long <- ic95 %>% gather(key = "bound", value = "x", ic_inf, ic_sup)
    p <- p %>%
      add_segments(data = ic_df_long, x = ~x, xend = ~x, y = 0, yend = ~y, 
                   frame = ~n, line = list(color = "blue", dash = "dash"), 
                   name = "95% CI")
  }
  
  p %>% layout(title = title,
               xaxis = list(title = x_label),
               yaxis = list(title = y_label))
}

#' Graficar KDE para Teorema del Límite Central
#'
#' @param df_resultados Data frame con resultados
#' @param media_global Media poblacional
#' @param sd_global Desviación estándar poblacional
#' @param tamaños Tamaños de muestra
#' @param name Nombre del activo
#' @param lang Idioma ("es", "en", "pt")
#' @return Objeto plotly
plot_kde_tlc <- function(df_resultados, media_global, sd_global, tamaños, name, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  dens_std <- df_resultados %>%
    group_by(n) %>%
    do({
      z <- (.$media - media_global) / (sd_global / sqrt(unique(.$n)))
      dens <- density(z)
      data.frame(x = dens$x, y = dens$y, n = unique(.$n))
    })
  
  norm_std <- lapply(tamaños, function(n) {
    x <- seq(-4, 4, length.out = 200)
    data.frame(x = x, y = dnorm(x), n = n)
  }) %>% bind_rows()
  
  line0 <- lapply(tamaños, function(n) {
    y_max <- max(dens_std$y[dens_std$n == n])
    data.frame(x = c(0,0), y = c(0,y_max), n = n)
  }) %>% bind_rows()
  
  # Textos traducidos
  title <- paste(t("tlc_title", lang), "-", name)
  x_label <- "Z (" %+% t("standardized_mean", lang) %+% ")"
  y_label <- t("density_label", lang)
  
  p <- plot_ly() %>%
    add_lines(data = dens_std, x = ~x, y = ~y, frame = ~n, 
              color = I("gold"), name = "KDE " %+% t("standardized", lang)) %>%
    add_lines(data = norm_std, x = ~x, y = ~y, frame = ~n, 
              color = I("red"), name = "N(0,1)") %>%
    add_lines(data = line0, x = ~x, y = ~y, frame = ~n, 
              color = I("black"), line = list(dash = 'dot'), 
              name = 'μ = 0') %>%
    layout(title = title,
           xaxis = list(title = x_label),
           yaxis = list(title = y_label))
  
  p
}

#' Graficar intervalo de confianza
#'
#' @param inf_res Resultados de inferencia
#' @param conf_level Nivel de confianza
#' @param lang Idioma ("es", "en", "pt")
#' @return Objeto plotly
plot_confidence_interval <- function(inf_res, conf_level, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  df <- data.frame(
    Método = t("asymptotic", lang),
    Media = inf_res$media_ret,
    Lower = inf_res$ci_asymptotic[1],
    Upper = inf_res$ci_asymptotic[2]
  )
  
  # Textos traducidos
  title <- paste(t("ci_title", lang), conf_level, "%")
  x_label <- t("return_mean_label", lang)
  
  plot_ly(df) %>%
    add_segments(x = ~Lower, xend = ~Upper, y = ~Método, yend = ~Método,
                 line = list(color = "blue", width = 3), name = "CI") %>%
    add_markers(x = ~Media, y = ~Método, 
                marker = list(color = "red", size = 10), name = t("mean", lang)) %>%
    layout(
      title = title,
      xaxis = list(title = x_label),
      yaxis = list(title = ""),
      showlegend = TRUE
    )
}

#' Graficar test t en ventanas móviles
#'
#' @param rolling_t Data frame con resultados móviles
#' @param window_size Tamaño de ventana
#' @param lang Idioma ("es", "en", "pt")
#' @return Objeto plotly
plot_rolling_t_test <- function(rolling_t, window_size, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  if(nrow(rolling_t) > 0) {
    
    # Textos traducidos
    title1 <- t("rolling_test_title", lang)
    y_label1 <- t("t_statistic", lang)
    x_label <- t("window_start", lang)
    title2 <- t("p_value_chart", lang)
    y_label2 <- t("p_value", lang)
    
    p1 <- plot_ly(rolling_t, x = ~window_start, y = ~t_stat, 
                  type = 'scatter', mode = 'lines',
                  name = t("t_statistic", lang), 
                  line = list(color = 'blue')) %>%
      add_lines(y = 1.96, name = '95% ' %+% t("limit", lang), 
                line = list(color = 'red', dash = 'dash')) %>%
      add_lines(y = -1.96, name = '', 
                line = list(color = 'red', dash = 'dash')) %>%
      layout(yaxis = list(title = y_label1),
             xaxis = list(title = x_label),
             title = title1)
    
    p2 <- plot_ly(rolling_t, x = ~window_start, y = ~p_value, 
                  type = 'scatter', mode = 'lines',
                  name = t("p_value", lang), 
                  line = list(color = 'green')) %>%
      add_lines(y = 0.05, name = 'α = 0.05', 
                line = list(color = 'orange', dash = 'dash')) %>%
      layout(yaxis = list(title = y_label2),
             xaxis = list(title = x_label),
             title = title2)
    
    subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
      layout(title = paste(t("dynamic_analysis", lang), 
                           t("window_size", lang), ":", window_size, 
                           t("days", lang)))
  } else {
    plot_ly() %>%
      add_annotations(
        text = t("window_too_large", lang),
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 16)
      )
  }
}

#' Graficar distribución bootstrap
#'
#' @param boot_means Medias bootstrap
#' @param inf_res Resultados de inferencia
#' @param R Número de réplicas
#' @param lang Idioma ("es", "en", "pt")
#' @return Objeto plotly
plot_bootstrap_distribution <- function(boot_means, inf_res, R, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  # Histograma de bootstrap
  hist_data <- hist(boot_means, plot = FALSE)
  
  # Curva normal teórica
  x_seq <- seq(min(boot_means), max(boot_means), length.out = 100)
  y_norm <- dnorm(x_seq, mean = inf_res$media_ret, 
                  sd = inf_res$sd_ret/sqrt(inf_res$n))
  y_norm <- y_norm * diff(hist_data$mids[1:2]) * length(boot_means)
  
  # Crear línea vertical para la media observada
  line_df <- data.frame(
    x = c(inf_res$media_ret, inf_res$media_ret),
    y = c(0, max(hist_data$counts))
  )
  
  # Textos traducidos
  title <- paste(t("bootstrap_dist_title", lang), "(R =", R, ")")
  x_label <- t("bootstrap_mean_label", lang)
  y_label <- t("frequency_label", lang)
  
  plot_ly() %>%
    add_bars(x = hist_data$mids, y = hist_data$counts, 
             name = "Bootstrap", 
             marker = list(color = 'rgba(100,149,237,0.7)')) %>%
    add_lines(x = x_seq, y = y_norm, name = t("theoretical_normal", lang),
              line = list(color = 'red', width = 2)) %>%
    add_lines(data = line_df, x = ~x, y = ~y, 
              name = t("observed_mean", lang),
              line = list(color = 'green', dash = 'dash')) %>%
    layout(
      title = title,
      xaxis = list(title = x_label),
      yaxis = list(title = y_label),
      showlegend = TRUE
    )
}

#' Graficar comparación de métodos
#'
#' @param inf_res Resultados de inferencia
#' @param boot_res Resultados de bootstrap
#' @param conf_level Nivel de confianza
#' @param lang Idioma ("es", "en", "pt")
#' @return Objeto plotly
plot_methods_comparison <- function(inf_res, boot_res, conf_level, lang = "es") {
  
  if(!exists("t")) {
    source("R/i18n.R", local = TRUE)
  }
  
  ci_boot <- boot_res$ci_boot$percent
  
  if(!is.null(ci_boot) && length(ci_boot) >= 5) {
    boot_ci_lower <- ci_boot[4]
    boot_ci_upper <- ci_boot[5]
    
    df <- data.frame(
      Método = c(t("asymptotic", lang), t("bootstrap_percentile", lang)),
      Media = c(inf_res$media_ret, mean(boot_res$boot_means)),
      Lower = c(inf_res$ci_asymptotic[1], boot_ci_lower),
      Upper = c(inf_res$ci_asymptotic[2], boot_ci_upper)
    )
    
    # Textos traducidos
    title <- paste(t("comparison_plot_title", lang), conf_level, "%")
    x_label <- t("return_mean_label", lang)
    y_label <- t("method_label", lang)
    
    plot_ly(df) %>%
      add_segments(x = ~Lower, xend = ~Upper, y = ~Método, yend = ~Método,
                   color = ~Método, colors = c("blue", "orange"), 
                   line = list(width = 3), showlegend = FALSE) %>%
      add_markers(x = ~Media, y = ~Método, color = ~Método,
                  colors = c("blue", "orange"),
                  marker = list(size = 10), showlegend = FALSE) %>%
      layout(
        title = title,
        xaxis = list(title = x_label),
        yaxis = list(title = y_label),
        shapes = list(
          list(type = "line", x0 = 0, x1 = 0, 
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = "gray", dash = "dash"))
        )
      )
  } else {
    plot_ly() %>%
      add_annotations(
        text = t("bootstrap_ci_error", lang),
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 16)
      )
  }
}

#' Función helper para concatenar strings
`%+%` <- function(string1, string2) {
  paste0(string1, string2)
}