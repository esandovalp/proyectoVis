install.packages("DT")

library(shiny)
library(tidyverse)
library(DT)
library(scales)

# UI Definition
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4),
  titlePanel("Family Background Analysis - ENPOL 2021"),
  
  verbatimTextOutput("debug_message"),
  
  tabsetPanel(
    # Tab 1: Living Situation Analysis
    tabPanel("Living Situation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("comparison_group", "Compare by:",
                             choices = c("Gender" = "SEXO",
                                         "Indigenous Language" = "P1_31_1",
                                         "Self-Identification" = "P1_32")),
                 hr(),
                 helpText("Note: Gender (1 = Male, 2 = Female)"),
                 helpText("Indigenous Language & Self-ID (1 = Yes, 2 = No)")
               ),
               mainPanel(
                 plotOutput("living_situation_plot", height = "400px"),
                 DTOutput("living_situation_table")
               )
             )
    ),
    
    # Tab 2: Violence/Precariousness
    tabPanel("Violence & Precariousness",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("violence_types", "Select Experiences:",
                                    choices = c("Physical Violence" = "P1_41_1",
                                                "Emotional Violence" = "P1_41_2",
                                                "Economic Hardship" = "P1_41_3",
                                                "Abandonment" = "P1_41_4")),
                 selectInput("group_filter", "Filter by:",
                             choices = c("All", "Women Only", "Indigenous Only"))
               ),
               mainPanel(
                 plotOutput("violence_plot", height = "400px"),
                 plotOutput("violence_breakdown", height = "300px")
               )
             )
    ),
    
    # Tab 3: Security Perception
    tabPanel("Security Perception",
             sidebarLayout(
               sidebarPanel(
                 selectInput("security_comparison", "Compare by:",
                             choices = c("Gender" = "SEXO",
                                         "Indigenous Language" = "P1_31_1",
                                         "Self-Identification" = "P1_32")),
                 sliderInput("age_range", "Age Range:",
                             min = 15, max = 80, value = c(15, 80))
               ),
               mainPanel(
                 plotOutput("security_perception_plot", height = "400px"),
                 plotOutput("security_trends", height = "300px")
               )
             )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Debug message output
  output$debug_message <- renderPrint({
    cat("Working Directory:", getwd(), "\n")
    data_path <- "data/ENPOL2021_SOC.csv"  # Changed from "../data/ENPOL2021_SOC.csv"
    cat("Looking for file:", normalizePath(data_path, mustWork = FALSE), "\n")
    cat("File exists:", file.exists(data_path), "\n")
    
    # List available files
    cat("\nFiles in current directory:", paste(list.files(), collapse=", "), "\n")
    cat("Files in parent directory:", paste(list.files(".."), collapse=", "), "\n")
    if(dir.exists("data")) {  # Changed from "../data"
      cat("Files in data directory:", paste(list.files("data"), collapse=", "), "\n")
    } else {
      cat("Data directory not found\n")
    }
  })
  
  # Modified data loading with error handling
  enpol_data <- reactive({
    read.csv("data/ENPOL2021_SOC.csv", encoding = "latin1")  # Changed from "../data/ENPOL2021_SOC.csv"
  })
  
  # Living Situation Analysis
  output$living_situation_plot <- renderPlot({
    req(enpol_data())
    data <- enpol_data()
    
    # Create living situation analysis
    data %>%
      group_by(!!sym(input$comparison_group)) %>%
      summarise(
        with_parents = mean(P1_8 == 1, na.rm = TRUE),
        with_relatives = mean(P1_8 == 2, na.rm = TRUE),
        alone = mean(P1_8 == 3, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(with_parents, with_relatives, alone),
                   names_to = "living_situation",
                   values_to = "percentage") %>%
      ggplot(aes(x = !!sym(input$comparison_group), 
                 y = percentage,
                 fill = living_situation)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = percent) +
      labs(title = "Living Situation Distribution",
           x = "Group",
           y = "Percentage",
           fill = "Living Situation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Violence Experience Analysis
  output$violence_plot <- renderPlot({
    req(enpol_data(), input$violence_types)
    data <- enpol_data()
    
    if(input$group_filter == "Women Only") {
      data <- filter(data, SEXO == 2)
    } else if(input$group_filter == "Indigenous Only") {
      data <- filter(data, P1_31_1 == 1)
    }
    
    data %>%
      select(all_of(input$violence_types)) %>%
      pivot_longer(everything(),
                   names_to = "violence_type",
                   values_to = "experience") %>%
      group_by(violence_type) %>%
      summarise(
        percentage = mean(experience == 1, na.rm = TRUE)
      ) %>%
      ggplot(aes(x = reorder(violence_type, -percentage), y = percentage)) +
      geom_bar(stat = "identity", fill = "darkred") +
      scale_y_continuous(labels = percent) +
      labs(title = "Experience of Violence/Precariousness",
           x = "Type",
           y = "Percentage Affected") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Security Perception Analysis
  output$security_perception_plot <- renderPlot({
    req(enpol_data())
    data <- enpol_data()
    
    data %>%
      filter(P1_2 >= input$age_range[1], P1_2 <= input$age_range[2]) %>%
      group_by(!!sym(input$security_comparison)) %>%
      summarise(
        felt_secure = mean(P1_45 == 1, na.rm = TRUE),
        felt_insecure = mean(P1_45 == 2, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(felt_secure, felt_insecure),
                   names_to = "security_perception",
                   values_to = "percentage") %>%
      ggplot(aes(x = !!sym(input$security_comparison), 
                 y = percentage,
                 fill = security_perception)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = percent) +
      labs(title = "Security Perception During Childhood/Adolescence",
           x = "Group",
           y = "Percentage",
           fill = "Perception") +
      theme_minimal()
  })
  
  # Detailed table output
  output$living_situation_table <- renderDT({
    req(enpol_data())
    data <- enpol_data()
    
    summary_table <- data %>%
      group_by(!!sym(input$comparison_group)) %>%
      summarise(
        Total_Count = n(),
        With_Parents = sum(P1_8 == 1, na.rm = TRUE),
        With_Relatives = sum(P1_8 == 2, na.rm = TRUE),
        Alone = sum(P1_8 == 3, na.rm = TRUE)
      )
    
    datatable(summary_table,
              options = list(pageLength = 10),
              rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)