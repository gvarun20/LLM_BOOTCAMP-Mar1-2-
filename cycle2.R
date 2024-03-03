library(shiny)

ui <- fluidPage(
  titlePanel("Simple Shiny App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Select a number", 1)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    paste("You selected:", input$n)
  })
}

shinyApp(ui, server)
install.packages("package_name")
