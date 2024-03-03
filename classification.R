library(shiny)
library(DT)
library(ggplot2)
library(randomForest)

# Define UI
ui <- fluidPage(
  navbarPage(
    title = "Classification App",
    tabPanel("Explore Data", 
             sidebarLayout(
               sidebarPanel(
                 actionButton("loadData", "Load Iris Dataset")
               ),
               mainPanel(
                 DTOutput("table")
               )
             )
    ),
    tabPanel("Classification Model",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_classify", "Select Variable", choices = NULL),
                 actionButton("trainButton", "Train Model")
               ),
               mainPanel(
                 plotOutput("classifyPlot"),
                 textOutput("classifyInsight")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive dataset for classification
  classify_data <- reactiveVal(NULL)
  
  # Load iris dataset on button click
  observeEvent(input$loadData, {
    classify_data(iris)
    updateSelectInput(session, "variable_classify", choices = names(classify_data()))
  })
  
  # Render data table
  output$table <- renderDT({
    datatable(classify_data(), options = list(pageLength = 10))
  })
  
  # Classification model plot
  output$classifyPlot <- renderPlot({
    req(input$variable_classify, classify_data())
    # Use a simple decision tree classifier
    classify_model <- randomForest(Species ~ ., data = classify_data())
    # Plot the importance of each variable
    importance_plot <- barplot(importance(classify_model)[, 1], main = "Variable Importance",
                               names.arg = names(importance(classify_model)[, 1]), col = "skyblue")
    text(importance_plot, 1, round(importance(classify_model)[, 1], 2), pos = 3, col = "red")
  })
  
  # Add insight about the Classification model plot
  output$classifyInsight <- renderText({
    "Insight: This plot shows the importance of each variable in the classification model."
  })
  
  # Train model on button click
  observeEvent(input$trainButton, {
    req(input$variable_classify, classify_data())
    classify_model <- randomForest(Species ~ ., data = classify_data())
    updateSelectInput(session, "variable_classify", choices = names(classify_data()))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
