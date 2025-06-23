# -------------------------------------------
# Shiny App - Análisis de accidentes aéreos (1995–2024)
# Autor: Agostina Silva
# -------------------------------------------

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

# Carga del dataset
accidentes_raw <- read_csv("accidentes_baaa_1995_2024.csv")

# Limpieza
accidentes <- accidentes_raw %>%
  filter(!is.na(Date), !is.na(Operator), !is.na(Fatalities)) %>%
  mutate(
    Año = year(Date),
    Aerolinea = str_to_title(Operator),
    Avion = str_trim(Aircraft)
  )

# UI
ui <- fluidPage(
  titlePanel("✈️ Accidentes aéreos graves (1995–2024) - BAAA"),
  
  uiOutput("banner_dinamico"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("anio", "Seleccionar rango de años:",
                  min = 1995, max = 2024, value = c(2000, 2024), sep = "")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top Aerolíneas", plotOutput("grafico_top")),
        tabPanel("Accidentes por Año", plotOutput("grafico_linea")),
        tabPanel("Top Modelos de Avión", plotOutput("grafico_modelo")),
        tabPanel("Acerca del proyecto", 
                 h4("Planteo:"),
                 p("Este proyecto analiza los accidentes aéreos más graves ocurridos entre 1995 y 2024, basados en datos públicos del sitio BAAA."),
                 h4("Metodología:"),
                 p("Se importó el dataset en formato CSV, se limpiaron los datos con tidyverse y se visualizaron con ggplot2.")
        )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  datos_filtrados <- reactive({
    accidentes %>%
      filter(Año >= input$anio[1], Año <= input$anio[2])
  })
  
  resumen_reactivo <- reactive({
    datos <- datos_filtrados()
    
    peor_accidente <- datos %>% 
      filter(Fatalities == max(Fatalities, na.rm = TRUE)) %>% 
      slice(1)
    
    list(
      total_accidentes = nrow(datos),
      total_fatalidades = sum(datos$Fatalities, na.rm = TRUE),
      peor_fecha = format(peor_accidente$Date, "%b %d, %Y"),
      peor_fatalidades = peor_accidente$Fatalities,
      peor_aerolinea = peor_accidente$Aerolinea,
      peor_lugar = peor_accidente$Location
    )
  })
  
  output$banner_dinamico <- renderUI({
    r <- resumen_reactivo()
    
    tags$div(style = "background-color:#29b6f6; color:white; padding:30px; margin-bottom:20px; border-radius:10px;",
             tags$h2("Peor accidente en el período seleccionado:", style = "margin-top:0;"),
             tags$p(
               "El accidente más grave fue de ", tags$b(r$peor_aerolinea),
               " en ", r$peor_lugar, " el ", tags$b(r$peor_fecha),
               " con ", tags$b(r$peor_fatalidades), " fatalidades.",
               style = "font-size:16px;"
             ),
             tags$div(style = "display:flex; gap:60px; font-size:24px; margin-top:20px;",
                      tags$div(
                        tags$strong(format(r$total_accidentes, big.mark = ",")), tags$br(), "Accidentes fatales"
                      ),
                      tags$div(
                        tags$strong(format(r$total_fatalidades, big.mark = ",")), tags$br(), "Total de fatalidades"
                      ),
                      tags$div(
                        tags$strong(r$peor_fecha), tags$br(), paste0("Peor accidente (", r$peor_fatalidades, " muertos)")
                      )
             )
    )
  })
  
  # Gráfico 1: Top Aerolíneas
  output$grafico_top <- renderPlot({
    datos_filtrados() %>%
      group_by(Aerolinea) %>%
      summarise(Fatalidades = sum(Fatalities, na.rm = TRUE)) %>%
      arrange(desc(Fatalidades)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(Aerolinea, Fatalidades), y = Fatalidades)) +
      geom_col(fill = "firebrick", width = 0.7) +
      coord_flip() +
      labs(
        title = "🔴 Top 10 Aerolíneas con más Fatalidades",
        x = "Aerolínea",
        y = "Fatalidades"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "#222222"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14, color = "#333333"),
        axis.text.y = element_text(size = 14, color = "#333333"),
        plot.margin = margin(20, 30, 20, 30)
      )
  })
  
  # Gráfico 2: Accidentes por año
  output$grafico_linea <- renderPlot({
    datos_filtrados() %>%
      count(Año) %>%
      ggplot(aes(x = Año, y = n)) +
      geom_line(color = "steelblue", size = 1.5) +
      geom_point(color = "steelblue", size = 3) +
      labs(
        title = "📈 Accidentes Fatales por Año",
        x = "Año",
        y = "Cantidad de Accidentes"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "#222222"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, color = "#333333"),
        plot.margin = margin(20, 30, 20, 30)
      )
  })
  
  # Gráfico 3: Top modelos de avión
  output$grafico_modelo <- renderPlot({
    datos_filtrados() %>%
      group_by(Avion) %>%
      summarise(Fatalidades = sum(Fatalities, na.rm = TRUE)) %>%
      arrange(desc(Fatalidades)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(Avion, Fatalidades), y = Fatalidades)) +
      geom_col(fill = "darkgreen", width = 0.7) +
      coord_flip() +
      labs(
        title = "🛩️ Top 10 Modelos de Avión con más Fatalidades",
        x = "Modelo de Avión",
        y = "Fatalidades"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "#222222"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14, color = "#333333"),
        axis.text.y = element_text(size = 14, color = "#333333"),
        plot.margin = margin(20, 30, 20, 30)
      )
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)

