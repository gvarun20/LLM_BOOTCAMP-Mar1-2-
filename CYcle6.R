library(shiny)
library(DT)
library(ggplot2)
library(scales)
library(reshape2)
library(forecast)
library(zoo)

# Define UI
ui <- fluidPage(
  navbarPage(
    title = "Price Forecasting App",
    tabPanel("Upload CSV", 
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV file",
                           accept = c(".csv")),
                 tags$hr(),
                 helpText("Please upload a CSV file.")
               ),
               mainPanel(
                 DTOutput("table")
               )
             )
    ),
    tabPanel("Trend",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_trend", "Select Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("trendPlot"),
                 textOutput("trendInsight")
               )
             )
    ),
    tabPanel("Distribution",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_dist", "Select Variable", choices = NULL),
                 sliderInput("bins", "Number of Bins:", min = 1, max = 50, value = 10)
               ),
               mainPanel(
                 plotOutput("distPlot"),
                 textOutput("distInsight")
               )
             )
    ),
    tabPanel("Density",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_density", "Select Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("densityPlot"),
                 textOutput("densityInsight")
               )
             )
    ),
    tabPanel("Deviation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_deviation", "Select Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("deviationPlot"),
                 textOutput("deviationInsight")
               )
             )
    ),
    tabPanel("Relations",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_relation_x", "Select X Variable", choices = NULL),
                 selectInput("variable_relation_y", "Select Y Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("relationPlot"),
                 textOutput("relationInsight")
               )
             )
    ),
    tabPanel("Correlation",
             sidebarLayout(
               mainPanel(
                 plotOutput("correlationPlot"),
                 dataTableOutput("correlationTable"),
                 textOutput("correlationInsight")
               ),
               sidebarPanel()
             )
    ),
    tabPanel("SMA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_sma", "Select Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("smaPlot"),
                 dataTableOutput("smaTable"),
                 textOutput("smaInsight")
               )
             )
    ),
    tabPanel("ARIMA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_arima", "Select Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("acfPlot"),
                 plotOutput("pacfPlot"),
                 plotOutput("forecastPlot"),
                 dataTableOutput("arimaTable"),
                 textOutput("arimaInsight")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update variable choices based on uploaded data
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "variable_trend", choices = names(data()))
      updateSelectInput(session, "variable_dist", choices = names(data()))
      updateSelectInput(session, "variable_density", choices = names(data()))
      updateSelectInput(session, "variable_deviation", choices = names(data()))
      updateSelectInput(session, "variable_relation_x", choices = names(data()))
      updateSelectInput(session, "variable_relation_y", choices = names(data()))
      updateSelectInput(session, "variable_sma", choices = names(data()))
      updateSelectInput(session, "variable_arima", choices = names(data()))
    }
  })
  
  # Impute missing data and assign to a new variable
  transformed_data <- reactive({
    req(data())
    new_data <- data()
    for (i in seq_along(new_data)) {
      new_data[[i]] <- ifelse(is.na(new_data[[i]]), mean(new_data[[i]], na.rm = TRUE), new_data[[i]])
    }
    new_data
  })
  
  # Render data table
  output$table <- renderDT({
    datatable(transformed_data(), options = list(pageLength = 10))
  })
  
  # Trend plot
  output$trendPlot <- renderPlot({
    req(input$variable_trend)
    ggplot(transformed_data(), aes_string(x = seq_len(nrow(transformed_data())), y = input$variable_trend)) +
      geom_line() +
      labs(x = "Time", y = input$variable_trend, title = "Trend Analysis") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Add insight about the Trend plot
  output$trendInsight <- renderText({
    "Insight: This plot illustrates the trend of the selected variable over time."
  })
  
  # Distribution plot
  output$distPlot <- renderPlot({
    req(input$variable_dist)
    ggplot(transformed_data(), aes_string(x = input$variable_dist)) +
      geom_histogram(binwidth = diff(range(transformed_data()[[input$variable_dist]], na.rm = TRUE)) / input$bins) +
      labs(x = input$variable_dist, y = "Frequency", title = "Distribution Analysis") +
      theme_minimal()
  })
  
  # Add insight about the Distribution plot
  output$distInsight <- renderText({
    "Insight: This histogram displays the distribution of the selected variable."
  })
  
  # Density plot
  output$densityPlot <- renderPlot({
    req(input$variable_density)
    ggplot(transformed_data(), aes_string(x = input$variable_density)) +
      geom_density(fill = "skyblue", alpha = 0.6) +
      labs(x = input$variable_density, y = "Density", title = "Density Plot") +
      theme_minimal()
  })
  
  # Add insight about the Density plot
  output$densityInsight <- renderText({
    "Insight: This density plot visualizes the probability density function of the selected variable."
  })
  
  # Deviation plot
  output$deviationPlot <- renderPlot({
    req(input$variable_deviation)
    ggplot(transformed_data(), aes_string(y = input$variable_deviation)) +
      geom_boxplot() +
      labs(y = input$variable_deviation, title = "Deviation Analysis") +
      theme_minimal()
  })
  
  # Add insight about the Deviation plot
  output$deviationInsight <- renderText({
    "Insight: This boxplot shows the distribution of the selected variable and highlights any deviations from the norm."
  })
  
  # Relations plot
  output$relationPlot <- renderPlot({
    req(input$variable_relation_x, input$variable_relation_y)
    ggplot(transformed_data(), aes_string(x = input$variable_relation_x, y = input$variable_relation_y)) +
      geom_point() +
      labs(x = input$variable_relation_x, y = input$variable_relation_y, title = "Scatter Plot") +
      theme_minimal()
  })
  
  # Add insight about the Relations plot
  output$relationInsight <- renderText({
    "Insight: This scatter plot shows the relationship between the selected X and Y variables."
  })
  
  # Correlation plot
  output$correlationPlot <- renderPlot({
    req(transformed_data())
    correlation_matrix <- cor(transformed_data(), use = "pairwise.complete.obs")
    ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
      labs(title = "Correlation Plot") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 45, hjust = 1))
  })
  
  # Add insight about the Correlation plot
  output$correlationInsight <- renderText({
    "Insight: This heatmap displays the correlation between different variables in the dataset."
  })
  
  # Correlation table
  output$correlationTable <- renderDataTable({
    req(transformed_data())
    correlation_matrix <- round(cor(transformed_data(), use = "pairwise.complete.obs"), 2)
    correlation_df <- as.data.frame(correlation_matrix)
    colnames(correlation_df) <- rownames(correlation_matrix)
    rownames(correlation_df) <- colnames(correlation_matrix)
    correlation_df
  })
  
  # SMA plot
  output$smaPlot <- renderPlot({
    req(input$variable_sma)
    sma_data <- transformed_data() %>%
      mutate(SMA = zoo::rollmean(transformed_data()[[input$variable_sma]], k = 10, na.pad = TRUE, align = "right"))
    ggplot(sma_data, aes_string(x = seq_along(sma_data[[input$variable_sma]]), y = input$variable_sma)) +
      geom_line() +
      geom_line(aes(y = SMA), color = "red") +
      labs(x = "Time", y = input$variable_sma, title = "Simple Moving Average (SMA)") +
      theme_minimal()
  })
  
  # Add insight about the SMA plot
  output$smaInsight <- renderText({
    "Insight: This plot shows the Simple Moving Average (SMA) of the selected variable with a 10-period window."
  })
  
  # SMA table
  output$smaTable <- renderDataTable({
    req(input$variable_sma)
    sma_data <- transformed_data() %>%
      mutate(SMA = zoo::rollmean(transformed_data()[[input$variable_sma]], k = 10, na.pad = TRUE, align = "right"))
    sma_table <- data.frame(Time_ID = seq_len(nrow(sma_data)),
                            Actual_Value = sma_data[[input$variable_sma]],
                            SMA_Value = sma_data$SMA)
    sma_table
  })
  
  # ARIMA plot
  output$acfPlot <- renderPlot({
    req(input$variable_arima)
    acf_data <- acf(transformed_data()[[input$variable_arima]], lag.max = 20)
    plot(acf_data, main = "Autocorrelation Function (ACF)")
  })
  
  # Add insight about the ACF plot
  output$arimaInsight <- renderText({
    "Insight: This plot shows the Autocorrelation Function (ACF) of the selected variable."
  })
  
  output$pacfPlot <- renderPlot({
    req(input$variable_arima)
    pacf_data <- pacf(transformed_data()[[input$variable_arima]], lag.max = 20)
    plot(pacf_data, main = "Partial Autocorrelation Function (PACF)")
  })
  
  output$forecastPlot <- renderPlot({
    req(input$variable_arima)
    arima_model <- auto.arima(transformed_data()[[input$variable_arima]])
    forecast_data <- forecast(arima_model, h = 3)
    plot(forecast_data, main = "Forecast for Next 3 Periods")
  })
  
  # Add insight about the Forecast plot
  output$forecastInsight <- renderText({
    "Insight: This plot shows the forecast for the next 3 periods using the ARIMA model."
  })
  
  # ARIMA table
  output$arimaTable <- renderDataTable({
    req(input$variable_arima)
    arima_model <- auto.arima(transformed_data()[[input$variable_arima]])
    forecast_data <- forecast(arima_model, h = 3)
    arima_table <- data.frame(Time_ID = seq_len(3),
                              Forecasted_Value = forecast_data$mean)
    arima_table
  })
}

# Run the application
shinyApp(ui = ui, server = server)
