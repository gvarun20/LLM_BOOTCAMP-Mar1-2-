library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("CSV File Upload and Data Table with Trend"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      tags$hr(),
      helpText("Note: Please make sure the CSV file has a header."),
      selectInput("variable", "Select Variable for Trend", ""),
      tags$hr()
    ),
    mainPanel(
      dataTableOutput("table"),
      plotOutput("trendChart")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$table <- renderDataTable({
    datatable(data())
  })
  
  observe({
    updateSelectInput(session, "variable", choices = colnames(data()))
  })
  
  selectedVariableData <- reactive({
    req(input$variable)
    data()[, c(1, match(input$variable, colnames(data()))), drop = FALSE]
  })
  
  output$trendChart <- renderPlot({
    ggplot(selectedVariableData(), aes_string(x = names(selectedVariableData())[1], y = input$variable, group = 1)) +
      geom_line() +
      labs(x = names(selectedVariableData())[1], y = input$variable, title = "Time Series Trend") +
      theme_minimal()
  })
}

shinyApp(ui, server)