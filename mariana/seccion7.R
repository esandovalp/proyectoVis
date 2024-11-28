library(shiny)
library(leaflet)
library(tidyverse)

# Datos de coordenadas (puedes ajustarlos según fuentes confiables)
coords <- data.frame(
  NOM_ENT = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
              "Chihuahua", "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
              "Jalisco", "Estado de México", "Michoacán", "Morelos", "Nayarit", "Nuevo León",
              "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
              "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas",
              "Ciudad de México"),
  lat = c(21.885, 32.526, 24.144, 19.832, 16.757, 28.635, 27.058, 19.123, 24.027, 20.917, 17.439, 20.091,
          20.659, 19.359, 19.566, 18.926, 21.751, 25.686, 17.071, 19.041, 20.588, 19.666, 22.158, 24.808,
          29.072, 18.003, 23.741, 19.313, 19.173, 20.709, 22.770, 19.432),
  lng = c(-102.291, -116.991, -110.299, -90.538, -93.113, -106.089, -101.706, -104.825, -104.653, -101.256,
          -99.543, -98.762, -103.348, -99.650, -101.189, -99.070, -104.895, -100.316, -96.721, -98.206,
          -100.393, -88.327, -100.987, -107.394, -110.974, -92.919, -98.998, -98.239, -96.142, -89.070,
          -102.583, -99.133)
)

# Leer los datos de ENPOL
data_soc <- read.csv("C:/Users/maria/Documents/ITAM/Materias/9° Semestre/Ciencia de Datos Aplicada 1/ENPOL2021_SOC.csv", encoding = "latin1")

data_summary <- data_soc %>%
  filter(SEXO == 2 & !is.na(P1_24_1)) %>%  # Filtrar mujeres con respuestas válidas en P1_24_1
  group_by(NOM_ENT) %>%
  summarise(
    total_diagnosticadas = sum(P1_24_1 == 1, na.rm = TRUE),  # Mujeres diagnosticadas
    total_mujeres = n()
  ) %>%
  mutate(
    porcentaje_diagnosticadas = (total_diagnosticadas / total_mujeres) * 100
  ) %>%
  arrange(desc(total_diagnosticadas)) %>%
  left_join(coords, by = "NOM_ENT")  # Combinar con las coordenadas

# UI de la aplicación
ui <- fluidPage(
  titlePanel("Cuadro de Mando: Salud en Prisión"),
  tabsetPanel(
    tabPanel("Primera Página",
             fluidRow(
               column(6,
                      h4("Gráfica de Mujeres Diagnosticadas en Prisión"),
                      plotOutput("barPlot")
               ),
               column(6,
                      h4("Mapa Interactivo"),
                      leafletOutput("map")
               )
             )
    ),
    tabPanel("Segunda Página",
             h4("Tabla de Datos"),
             tableOutput("dataTable")
    )
  )
)

# Servidor de la aplicación
server <- function(input, output, session) {
  
  # Gráfica de barras
  output$barPlot <- renderPlot({
    ggplot(data_summary, aes(x = reorder(NOM_ENT, -total_diagnosticadas), y = total_diagnosticadas)) +
      geom_bar(stat = "identity", fill = "darkviolet", color = "darkviolet") +
      labs(
        x = "Entidad Federativa",
        y = "Número de Mujeres Diagnosticadas",
        title = "Mujeres Diagnosticadas con Enfermedades en Prisión"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  })
  
  # Mapa interactivo simulando mapa de calor
  output$map <- renderLeaflet({
    leaflet(data_summary) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng,
        lat = ~lat,
        weight = 1,
        radius = ~sqrt(total_diagnosticadas) * 10000,  # Escalar el tamaño proporcional
        color = "darkviolet",
        fillColor = "darkviolet",
        fillOpacity = 0.3,  # Transparencia para simular calor
        popup = ~paste(NOM_ENT, ": ", total_diagnosticadas, " diagnosticadas")
      )
  })
  
  # Tabla básica
  output$dataTable <- renderTable({
    data_summary %>% 
      select(NOM_ENT, total_diagnosticadas, total_mujeres, porcentaje_diagnosticadas) %>%
      arrange(desc(total_diagnosticadas))
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
