

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(fmsb)
library(reshape2)
library(DT)


# Load dataset (you can adjust the path as needed)
data <- read.csv("C:/Users/Hp/Desktop/Intersteller/Intersteller.csv")

# Data Cleaning
data_clean <- data %>%
  distinct(obj_ID, .keep_all = TRUE) %>%
  select(-obj_ID, -spec_obj_ID)

# Define the UI
ui <- dashboardPage(
  skin = "skyblue",
  
  dashboardHeader(title = "Interstellar Object Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset Overview", tabName = "overview", icon = icon("table")),
      menuItem("Class Distribution", tabName = "class_dist", icon = icon("bar-chart")),
      menuItem("Photometric Measurements", tabName = "photometric", icon = icon("chart-line")),
      menuItem("Redshift Analysis", tabName = "redshift", icon = icon("chart-area")),
      menuItem("Celestial Coordinates", tabName = "coordinates", icon = icon("globe")),
      menuItem("Correlation & Filters", tabName = "correlation", icon = icon("braille"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1: Dataset Overview
      tabItem(tabName = "overview",
              h3("Dataset Overview"),
              fluidRow(
                box(width = 12, dataTableOutput("data_summary"))
              )
      ),
      
      # Tab 2: Class Distribution
      tabItem(tabName = "class_dist",
              h3("Class Distribution"),
              fluidRow(
                box(title = "Class Distribution", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("class_dist_plot", height = "400px")),
                box(title = "Distribution of Object Classes", width = 6, solidHeader = TRUE, status = "primary",
                    verbatimTextOutput("class_summary"))
              )
      ),
      
      # Tab 3: Photometric Measurements
      tabItem(tabName = "photometric",
              h3("Photometric Measurements"),
              fluidRow(
                box(title = "Filter Distribution", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("filter_dist_plot", height = "400px")),
                box(title = "Density Plot by Filters", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("density_plot", height = "400px"))
              )
      ),
      
      # Tab 4: Redshift Analysis
      tabItem(tabName = "redshift",
              h3("Redshift Analysis"),
              fluidRow(
                box(title = "Redshift Histogram", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("redshift_hist", height = "400px")),
                box(title = "Redshift Boxplot by Class", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("redshift_boxplot", height = "400px"))
              )
      ),
      
      # Tab 5: Celestial Coordinates
      tabItem(tabName = "coordinates",
              h3("Celestial Coordinates Analysis"),
              fluidRow(
                box(title = "Alpha vs Delta (Scatter)", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("scatter_alpha_delta", height = "400px")),
                box(title = "Alpha vs Delta (Contour)", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("contour_alpha_delta", height = "400px"))
              )
      ),
      
      # Tab 6: Correlation & Filters
      tabItem(tabName = "correlation",
              h3("Correlation and Filter Analysis"),
              fluidRow(
                box(title = "Correlation Heatmap", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("correlation_heatmap", height = "400px")),
                box(title = "Radar Chart: Filter Averages by Class", width = 6, solidHeader = TRUE, status = "primary",
                    plotOutput("radar_plot", height = "400px"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Dataset Overview
  output$data_summary <- renderDataTable({
    datatable(data_clean, options = list(pageLength = 10))
  })
  
  # Class Distribution Plot
  output$class_dist_plot <- renderPlotly({
    p <- ggplot(data_clean, aes(x = class)) +
      geom_bar(fill = "skyblue") +
      theme_minimal() +
      ggtitle("Distribution of Object Classes")
    ggplotly(p)
  })
  
  # Class Summary
  output$class_summary <- renderPrint({
    table(data_clean$class)
  })
  
  # Filter Distribution Plot
  output$filter_dist_plot <- renderPlotly({
    p <- ggplot(data_clean, aes(x = u)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      theme_minimal() +
      ggtitle("Ultraviolet Filter (u) Distribution")
    ggplotly(p)
  })
  
  # Density Plot
  output$density_plot <- renderPlotly({
    data_long <- data_clean %>%
      pivot_longer(cols = c(u, g, r, i, z), names_to = "filter", values_to = "value")
    
    p <- ggplot(data_long, aes(x = value, color = filter)) +
      geom_density(size = 1) +
      facet_wrap(~filter, scales = "free_x") +  
      theme_minimal() +
      ggtitle("Density Plot of Photometric Filters")
    
    ggplotly(p)
  })
  
  # Redshift Histogram
  output$redshift_hist <- renderPlotly({
    p <- ggplot(data_clean, aes(x = redshift, fill = class)) +
      geom_histogram(bins = 30, alpha = 0.7) +
      theme_minimal() +
      ggtitle("Redshift Distribution by Object Class")
    ggplotly(p)
  })
  
  # Redshift Boxplot
  output$redshift_boxplot <- renderPlotly({
    p <- ggplot(data_clean, aes(x = class, y = redshift, fill = class)) +
      geom_boxplot() +
      theme_minimal() +
      ggtitle("Redshift Distribution by Object Class")
    ggplotly(p)
  })
  
  # Scatter Plot Alpha vs Delta
  output$scatter_alpha_delta <- renderPlotly({
    p <- ggplot(data_clean, aes(x = alpha, y = delta, color = class)) +
      geom_point(alpha = 0.5) +
      theme_minimal() +
      ggtitle("Alpha vs Delta")
    ggplotly(p)
  })
  
  # Contour Plot Alpha vs Delta
  output$contour_alpha_delta <- renderPlotly({
    p <- ggplot(data_clean, aes(x = alpha, y = delta, z = u)) +
      geom_density_2d() +
      theme_minimal() +
      ggtitle("Contour Plot of Alpha vs Delta with Ultraviolet Filter (u)")
    ggplotly(p)
  })
  
  # Correlation Heatmap
  output$correlation_heatmap <- renderPlotly({
    numeric_data <- data_clean %>% select(alpha, delta, u, g, r, i, z, redshift)
    corr_matrix <- cor(numeric_data)
    melted_corr <- melt(corr_matrix)
    
    p <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      ggtitle("Correlation Heatmap")
    
    ggplotly(p)
  })
  
  # Radar Plot
  output$radar_plot <- renderPlot({
    data_radar <- data_clean %>%
      group_by(class) %>%
      summarise(across(c(u, g, r, i, z), mean))
    
    data_radar <- rbind(rep(1, 5), rep(0, 5), data_radar[,-1])
    
    radarchart(data_radar, axistype = 1,
               pcol = c("blue", "green", "red"), 
               plwd = 2, plty = 1, 
               title = "Radar Chart: Average Filter Values by Object Class")
  })
}

# Run the application
shinyApp(ui = ui, server = server)


