# app.R - Archivo principal de la aplicación Shiny

# Cargar configuraciones globales
source("global.R")

# Cargar módulos
source("R/data_loader.R")
source("R/simulation_functions.R")
source("R/returns_processing.R")
source("R/inference.R")
source("R/bootstrap.R")
source("R/plot_functions.R")

# UI
ui <- fluidPage(
  # Incluir CSS personalizado
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel(
    div(
      img(src = "https://www.r-project.org/logo/Rlogo.png", height = "50px"),
      "Explorador LGN & TLC Interactivo con Inferencia Estadística"
    ),
    windowTitle = "Shiny TCL Finance"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Configuración de Datos"),
      textInput("ticker", "Ticker (Yahoo Finance):", value = "NVDA"),
      dateRangeInput("dates", "Rango de Fechas:", 
                     start = Sys.Date()-365, 
                     end = Sys.Date()),
      
      hr(),
      h4("Configuración de Simulación"),
      numericInput("replicates", "Réplicas por n:", value = 1000, min = 100, step = 100),
      textInput("tamaños", "Tamaños de Muestra (separados por coma):", 
                value = "5,10,20,30,40,50,60,70,80,90,100"),
      checkboxInput("show_ic_shaded", "Mostrar IC 95% sombreado", value = FALSE),
      
      hr(),
      h4("Configuración de Inferencia"),
      numericInput("conf_level", "Nivel de Confianza (%):", 
                   value = 95, min = 80, max = 99, step = 1),
      numericInput("window_size", "Tamaño Ventana Móvil (días):", 
                   value = 30, min = 5, max = 250, step = 5),
      numericInput("bootstrap_reps", "Réplicas Bootstrap:", 
                   value = 1000, min = 100, max = 10000, step = 100),
      
      hr(),
      actionButton("go", "Ejecutar Simulación", 
                   icon = icon("play"), 
                   class = "btn-primary btn-block"),
      
      # Información adicional
      hr(),
      div(class = "well",
          h5("Información:"),
          p("Esta aplicación demuestra conceptos estadísticos aplicados a finanzas."),
          p("Desarrollado por: Juan Felipe Echeverri")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        type = "pills",
        
        # Pestañas existentes
        tabPanel("Distribución de Precios", 
                 icon = icon("chart-bar"),
                 plotlyOutput("hist_plot", height = "500px")),
        
        tabPanel("Ley Grandes Números", 
                 icon = icon("project-diagram"),
                 plotlyOutput("kde_lgn", height = "600px")),
        
        tabPanel("Teorema Límite Central", 
                 icon = icon("braille"),
                 plotlyOutput("kde_tlc", height = "600px")),
        
        # Nueva pestaña para Inferencia Formal
        tabPanel("Inferencia Formal", 
                 icon = icon("calculator"),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4("Intervalos de Confianza"),
                            tableOutput("ci_table"),
                            plotlyOutput("ci_plot", height = "300px")
                          )
                   ),
                   column(6,
                          wellPanel(
                            h4("Test t: H₀: μ = 0"),
                            verbatimTextOutput("t_test_results"),
                            plotlyOutput("rolling_t_test", height = "300px")
                          )
                   )
                 )
        ),
        
        # Nueva pestaña para Bootstrap
        tabPanel("Bootstrap", 
                 icon = icon("random"),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4("Distribución Bootstrap"),
                            plotlyOutput("bootstrap_dist", height = "400px"),
                            tableOutput("bootstrap_summary")
                          )
                   ),
                   column(6,
                          wellPanel(
                            h4("Comparación de Métodos"),
                            plotlyOutput("comparison_plot", height = "400px"),
                            verbatimTextOutput("comparison_text")
                          )
                   )
                 )
        ),
        
        # Pestaña de información
        tabPanel("Información", 
                 icon = icon("info-circle"),
                 includeMarkdown("README.md"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive para tamaños de muestra
  tamaños_vec <- reactive({
    vals <- strsplit(input$tamaños, ",")[[1]] %>% trimws()
    as.integer(vals[vals != ""])
  })
  
  # Cargar datos
  prices_data <- eventReactive(input$go, {
    req(input$ticker)
    load_stock_data(input$ticker, input$dates[1], input$dates[2])
  })
  
  # Resultados de simulación
  sim_results <- eventReactive(input$go, {
    dat <- prices_data()
    req(dat)
    perform_simulation(dat$prices, tamaños_vec(), input$replicates, dat$name)
  })
  
  # Resultados de inferencia
  inference_results <- eventReactive(input$go, {
    dat <- prices_data()
    req(dat$returns)
    perform_inference_analysis(dat$returns, input$conf_level/100, input$window_size)
  })
  
  # Resultados de bootstrap
  bootstrap_results <- eventReactive(input$go, {
    inf_res <- inference_results()
    req(inf_res$returns)
    perform_bootstrap_analysis(inf_res$returns, input$bootstrap_reps, inf_res$conf_level)
  })
  
  # ===== OUTPUTS =====
  
  # Histograma
  output$hist_plot <- renderPlotly({
    dat <- prices_data()
    req(dat)
    plot_histogram(dat$prices, dat$name)
  })
  
  # KDE LGN
  output$kde_lgn <- renderPlotly({
    res <- sim_results()
    req(res)
    plot_kde_lgn(
      res$df_resultados, 
      res$media_global, 
      res$sd_global, 
      res$tamaños,
      input$show_ic_shaded,
      res$name
    )
  })
  
  # KDE TLC
  output$kde_tlc <- renderPlotly({
    res <- sim_results()
    req(res)
    plot_kde_tlc(
      res$df_resultados, 
      res$media_global, 
      res$sd_global, 
      res$tamaños,
      res$name
    )
  })
  
  # Tabla IC
  output$ci_table <- renderTable({
    inf_res <- inference_results()
    req(inf_res)
    create_ci_table(inf_res, input$conf_level)
  }, digits = 6)
  
  # Gráfico IC
  output$ci_plot <- renderPlotly({
    inf_res <- inference_results()
    req(inf_res)
    plot_confidence_interval(inf_res, input$conf_level)
  })
  
  # Resultados test t
  output$t_test_results <- renderPrint({
    inf_res <- inference_results()
    req(inf_res$t_test)
    print_t_test_results(inf_res$t_test, inf_res$conf_level)
  })
  
  # Test t móvil
  output$rolling_t_test <- renderPlotly({
    inf_res <- inference_results()
    req(inf_res$rolling_t)
    plot_rolling_t_test(inf_res$rolling_t, input$window_size)
  })
  
  # Distribución bootstrap
  output$bootstrap_dist <- renderPlotly({
    boot_res <- bootstrap_results()
    inf_res <- inference_results()
    req(boot_res, inf_res)
    plot_bootstrap_distribution(boot_res$boot_means, inf_res, input$bootstrap_reps)
  })
  
  # Resumen bootstrap
  output$bootstrap_summary <- renderTable({
    boot_res <- bootstrap_results()
    req(boot_res)
    inf_res <- inference_results()
    create_bootstrap_summary(boot_res, inf_res)
  }, digits = 6)
  
  # Comparación
  output$comparison_plot <- renderPlotly({
    inf_res <- inference_results()
    boot_res <- bootstrap_results()
    req(inf_res, boot_res)
    plot_methods_comparison(inf_res, boot_res, input$conf_level)
  })
  
  output$comparison_text <- renderPrint({
    inf_res <- inference_results()
    boot_res <- bootstrap_results()
    print_comparison_analysis(inf_res, boot_res)
  })
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)