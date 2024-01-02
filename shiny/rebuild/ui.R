library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(FLCore)
library(FLBRP)
library(FLasher)

data("ple4brp")

source("~/Desktop/flr/FLCandy/R/rebuild.R")

# Assuming the provided functions are loaded in the R environment

# UI
ui <- fluidPage(
  titlePanel("Fisheries Stock Rebuilding"),
  sidebarLayout(
    sidebarPanel(
      numericInput("targetB", "Target SSB:", value = 50000),
      numericInput("targetF", "Target F:", value = 0),
      numericInput("n", "Number of Iterations:", value = 50),
      numericInput("burnin", "Burn-in Iterations:", value = 20),
      checkboxInput("truncate", "Truncate Result", value = TRUE),
      actionButton("runBtn", "Run")
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

# Server
server <- function(input, output) {
  
  results <- eventReactive(input$runBtn, {
    # Assuming the provided functions are loaded in the R environment
    rebuilt_stock <- rebuild(ple4brp, targetB = input$targetB, targetF = input$targetF,
                             n = input$n, burnin = input$burnin, truncate = input$truncate)
    
    # You can customize this part based on your specific results and plotting requirements
    return(data.frame(Year = dimnames(rebuilt_stock)[[1]],
                      SSB = ssb(rebuilt_stock)[, 1]))
  })
  
  output$plot1 <- renderPlot({
    ggplot(results(), aes(x = Year, y = SSB)) +
      geom_line() +
      labs(title = "Rebuilt Stock Over Time", x = "Year", y = "SSB")
  })
}

# Run the Shiny app
shinyApp(ui, server)
