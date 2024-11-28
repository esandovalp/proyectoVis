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
data <- read.csv("C:/Users/maria/Documents/ITAM/Materias/9° Semestre/Ciencia de Datos Aplicada 1/ENPOL2021_6.csv", encoding = "latin1")

# Resumen de datos
data_summary <- data %>%
  filter(!is.na(P6_1)) %>%
  group_by(NOM_ENT) %>%
  summarise(total_mujeres = sum(P6_1 == 1, na.rm = TRUE)) %>%
  arrange(desc(total_mujeres)) %>%
  left_join(coords, by = "NOM_ENT")  # Combinar con coordenadas

# UI de la aplicación
ui <- fluidPage(
  titlePanel("Maternidad en prisión"),
  tabsetPanel(
    tabPanel("Primera Página",
             fluidRow(
               column(6,
                      h4("Gráfica de mujeres con hijos en prisión"),
                      plotOutput("barPlot")
               ),
               column(6,
                      h4("Mapa Interactivo"),
                      leafletOutput("map")
               )
             )
    ),
    tabPanel("Segunda Página",
             h4("Tabla Interactiva"),
             tableOutput("dataTable")
    )
  )
)

# Servidor de la aplicación
server <- function(input, output, session) {
  
  # Gráfica de barras
  output$barPlot <- renderPlot({
    ggplot(data_summary, aes(x = reorder(NOM_ENT, -total_mujeres), y = total_mujeres)) +
      geom_bar(stat = "identity", fill = "darkviolet", color = "darkviolet") +
      labs(
        x = "Entidad Federativa",
        y = "Número de Mujeres",
        title = "Mujeres que Viven con Hijos en Prisión"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  })
  
  # Mapa interactivo
  output$map <- renderLeaflet({
    leaflet(data_summary) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~sqrt(total_mujeres),
        color = "darkviolet",
        fillOpacity = 0.6,
        popup = ~paste(NOM_ENT, ": ", total_mujeres, " mujeres")
      )
  })
  
  # Tabla básica
  output$dataTable <- renderTable({
    data %>% select(ID_PER, NOM_ENT, SEXO, FUERO, P6_1) %>% head(20)
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
