## ✈️ Introducción

Este informe analiza los accidentes aéreos con al menos una fatalidad entre 1995 y 2024, utilizando datos recolectados del sitio [BAAA](https://www.baaa-acro.com/statistics/worst-crashes) mediante un proceso de scraping automatizado (ver script scrapper.R).

A través del análisis, se busca responder:

  ¿Qué aerolíneas han estado más involucradas en accidentes fatales?

  ¿Existen patrones temporales en la ocurrencia de estos eventos?

  ¿Qué modelos de avión se repiten con mayor frecuencia en este tipo de accidentes?

El objetivo es identificar tendencias o características comunes entre los casos más graves, sin asumir de antemano a qué tipo de operadores o aeronaves corresponden.

## 📥 Lectura e inspección de datos

```{r}
accidentes_raw <- read_csv("accidentes_baaa_1995_2024.csv")
glimpse(accidentes_raw)
```

## 🧹 Limpieza y transformación

Se realiza limpieza de columnas clave: fechas, operadores, nombres de aviones, etc.

```{r}
accidentes <- accidentes_raw %>%
  filter(!is.na(Date), !is.na(Operator), !is.na(Fatalities)) %>%
  mutate(
    Date = as.Date(Date),
    Año = year(Date),
    Aerolinea = str_to_title(str_trim(Operator)),
    Avion = str_to_title(str_trim(Aircraft)),
    Categoria = case_when(
      Fatalities >= 100 ~ "Masivo",
      Fatalities >= 50 ~ "Grave",
      TRUE ~ "Moderado"
    )
  ) %>%
  select(Date, Año, Aerolinea, Avion, Location, Fatalities, Categoria)
```

## 📊 Visualización de los datos

### 🔝 Top 10 Aerolíneas con más Fatalidades

```{r}
accidentes %>%
  group_by(Aerolinea) %>%
  summarise(Fatalidades = sum(Fatalities, na.rm = TRUE)) %>%
  arrange(desc(Fatalidades)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Aerolinea, Fatalidades), y = Fatalidades)) +
  geom_col(fill = "#D7263D") +
  coord_flip() +
  labs(title = "Top 10 Aerolíneas con más Fatalidades", x = "Aerolínea", y = "Fatalidades") +
  theme_minimal()
```

### 📈 Accidentes por Año

```{r}
accidentes %>%
  count(Año) %>%
  ggplot(aes(x = Año, y = n)) +
  geom_line(color = "#1B98E0", size = 1.2) +
  geom_point(color = "#1B98E0", size = 2) +
  labs(title = "Accidentes fatales por Año", x = "Año", y = "Cantidad") +
  theme_minimal()
```

## 🛬 Modelos de Avión con más Fatalidades

```{r}
accidentes %>%
  group_by(Avion) %>%
  summarise(Fatalidades = sum(Fatalities, na.rm = TRUE)) %>%
  arrange(desc(Fatalidades)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Avion, Fatalidades), y = Fatalidades)) +
  geom_col(fill = "#2A9D8F") +
  coord_flip() +
  labs(title = "Top 10 Modelos de Avión con más Fatalidades", x = "Modelo", y = "Fatalidades") +
  theme_minimal()
```

## 🧠 Conclusiones

Las aerolíneas con mayor cantidad de fatalidades en accidentes aéreos tienden a ser operadores pequeños, estatales o vinculados a contextos militares, como Islamic Revolutionary Guard Corps o African Air. Esto sugiere que los accidentes más graves no siempre involucran a grandes aerolíneas comerciales, sino que pueden estar relacionados con condiciones operativas precarias, entornos inestables o conflictos regionales.

Pese a la variabilidad interanual, la línea de tendencia indica una disminución sostenida en la cantidad de accidentes fatales entre 1995 y 2024. Se observan picos en ciertos períodos que podrían coincidir con situaciones geopolíticas específicas o aumentos del tráfico aéreo.

En cuanto a los modelos de avión, los que acumulan más fatalidades corresponden en su mayoría a aeronaves de transporte militar o de carga pesada, como el Antonov AN-32 y el Ilyushin Il-76, lo cual refleja la alta capacidad de estos aviones y su frecuente uso en operaciones de riesgo.

## ⚠️ Obstáculos enfrentados

Durante el procesamiento del dataset:

- Se detectaron más de 600 filas con valores faltantes en el nombre de la aerolínea (Operator), lo que motivó su exclusión.

- Aunque no se encontraron inconsistencias por espacios extra ni duplicados exactos en mayúsculas/minúsculas, se decidió estandarizar visualmente los nombres con str_to_title() para mejorar la presentación de los gráficos.

También se aplicó str_trim() como medida preventiva para evitar errores silenciosos.

- Originalmente se consideraron fuentes como ASN o Kaggle, pero debido a sus restricciones de uso y limitaciones para automatizar el acceso (como la necesidad de solicitar los datos por correo), se optó por obtener la información desde BAAA, que publica datos de forma accesible. A partir de allí, se implementó un scraper personalizado en scrapper.R para recorrer las páginas y extraer los registros relevantes.

## 📎 App interactiva

Para una exploración visual y flexible, se desarrolló una [Shiny App interactiva](https://datatalesbyagos.shinyapps.io/fatalidades_aereas_1995_2024/) que permite filtrar por rango de años y ver los top 10 en distintos aspectos.

La app fue construida usando:

shiny para la interfaz reactiva,

dplyr para la manipulación de datos,

ggplot2 para las visualizaciones,

stringr y lubridate para limpieza y tratamiento de texto y fechas.
