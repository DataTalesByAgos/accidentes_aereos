# -------------------------------
# Scraping de accidentes aéreos graves (BAAA)
# Fuente: https://www.baaa-acro.com/statistics/worst-crashes
# Periodo objetivo: 1995–2024
# Librerías requeridas: rvest, httr, dplyr, lubridate, stringr
# -------------------------------

# Cargar librerías necesarias
library(rvest)
library(httr)         
library(dplyr)
library(lubridate)
library(stringr)

# -------------------------------
# Función para scrapear una página específica del sitio BAAA
# -------------------------------
scrape_baaa_page <- function(page_num) {
  url <- paste0("https://www.baaa-acro.com/statistics/worst-crashes?page=", page_num)
  cat("Scrapeando página:", page_num, "\n")
  
  # Usar un user-agent personalizado para respetar buenas prácticas
  res <- GET(url, user_agent("MiScraper/1.0 (contacto@ejemplo.com)"))
  
  if (status_code(res) != 200) return(NULL)
  
  # Parsear contenido HTML
  html <- read_html(res)
  rows <- html %>% html_nodes("table tr")
  
  # Extraer datos de cada fila de la tabla
  data <- lapply(rows, function(row) {
    cols <- row %>% html_nodes("td")
    if (length(cols) < 6) return(NULL)  # Filas sin suficientes columnas son ignoradas
    
    date <- cols[2] %>% html_text(trim = TRUE)
    operator <- cols[3] %>% html_text(trim = TRUE)
    aircraft <- cols[4] %>% html_text(trim = TRUE)
    location <- cols[5] %>% html_text(trim = TRUE)
    fatalities <- cols[6] %>% html_text(trim = TRUE)
    registration <- cols[7] %>% html_text(trim = TRUE)
    
    # Retornar como lista nombrada
    list(
      Date = date,
      Operator = operator,
      Aircraft = aircraft,
      Location = location,
      Fatalities = as.integer(fatalities),
      Registration = registration
    )
  })
  
  # Convertir listas en un dataframe
  return(bind_rows(data))
}

# -------------------------------
# Scraping de múltiples páginas
# El sitio tiene ~367 páginas, pero podés aumentar el rango si lo deseás.
# -------------------------------
all_data <- bind_rows(lapply(0:50, scrape_baaa_page))  # Recomendado hacer pruebas incrementales

# -------------------------------
# Limpieza y filtrado de los datos
# -------------------------------
all_data <- all_data %>%
  mutate(
    # Convertir fechas en formato Date
    Date = parse_date_time(Date, orders = c("b d, Y", "B d, Y"), locale = "C")
  ) %>%
  # Filtrar por fechas válidas y dentro del rango deseado
  filter(!is.na(Date), year(Date) >= 1995, year(Date) <= 2024)

# -------------------------------
# Exportar los datos a CSV
# -------------------------------
write.csv(all_data, "accidentes_baaa_1995_2024.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Mensaje final
cat("✅ CSV guardado con", nrow(all_data), "accidentes entre 1995 y 2024.\n")


