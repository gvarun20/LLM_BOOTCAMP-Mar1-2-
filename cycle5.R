library(shiny)
library(DT)
library(ggplot2)
library(scales)
library(corrplot)

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
                 helpText("Please upload a CSV file."),
                 selectInput("variable", "Select Variable", choices = NULL)
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
                 plotOutput("trendPlot")
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
                 plotOutput("distPlot")
               )
             )
    ),
    tabPanel("Density",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_density", "Select Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("densityPlot")
               )
             )
    ),
    tabPanel("Scatter Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_x", "Select X Variable", choices = NULL),
                 selectInput("variable_y", "Select Y Variable", choices = NULL),
                 radioButtons("color_var", "Select Color Variable", choices = NULL)
               ),
               mainPanel(
                 plotOutput("scatterPlot")
               )
             )
    ),
    tabPanel("Correlation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("corr_method", "Correlation Method",
                             choices = c("pearson", "spearman"),
                             selected = "pearson")
               ),
               mainPanel(
                 DTOutput("correlationTable"),
                 plotOutput("correlationPlot")
               )
             )
    ),
    tabPanel("SMA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_sma", "Select Variable", choices = NULL),
                 sliderInput("sma_window", "SMA Window Size:", min = 1, max = 50, value = 5)
               ),
               mainPanel(
                 plotOutput("smaPlot"),
                 DTOutput("smaTable")
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
      updateSelectInput(session, "variable", choices = names(data()))
      updateSelectInput(session, "variable_trend", choices = names(data()))
      updateSelectInput(session, "variable_dist", choices = names(data()))
      updateSelectInput(session, "variable_density", choices = names(data()))
      updateSelectInput(session, "variable_x", choices = names(data()))
      updateSelectInput(session, "variable_y", choices = names(data()))
      updateSelectInput(session, "color_var", choices = c("None", names(data())))
      updateSelectInput(session, "variable_sma", choices = names(data()))
    } else {
      # If data is NULL, set default choices to avoid the error
      updateSelectInput(session, "variable", choices = "")
      updateSelectInput(session, "variable_trend", choices = "")
      updateSelectInput(session, "variable_dist", choices = "")
      updateSelectInput(session, "variable_density", choices = "")
      updateSelectInput(session, "variable_x", choices = "")
      updateSelectInput(session, "variable_y", choices = "")
      updateSelectInput(session, "color_var", choices = "None")
      updateSelectInput(session, "variable_sma", choices = "")
    }
  })
  
  # ... (rest of the server code)
}

# Run the application
shinyApp(ui = ui, server = server)
