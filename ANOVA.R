# Load the required libraries
library(shiny)
library(DT)
library(car)
library(multcomp)
library(tidyr)
library(ggplot2)  # Added for boxplot visualization

# Define the UI
ui <- fluidPage(
  titlePanel("ANOVA App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      textAreaInput("pasteData", "Paste Data from Clipboard", rows = 10),
      actionButton("loadData", "Load Data"),
      
      # ANOVA options
      selectInput("factorVar", "Select Factor Variable", ""),
      selectInput("responseVar", "Select Response Variable", ""),
      actionButton("runANOVA", "Run ANOVA"),
    ),
    mainPanel(
      DTOutput("dataTable"),
      verbatimTextOutput("anovaSummary"),
      verbatimTextOutput("assumptionsSummary"),
      verbatimTextOutput("tukeySummary"),
      
      # Boxplot visualization
      plotOutput("boxplot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  anovaModel <- reactiveVal(NULL)
  factorLevels <- reactiveVal(NULL)
  tukeyResult <- reactiveVal(NULL)
  
  # Set the default dataset
  defaultAovData <- as.data.frame(read.csv("data//ads.csv", sep = ";"))
  defaultAovData <- as.data.frame(pivot_longer(defaultAovData, cols = 2:4, names_to = "AdPlacement",
                                               values_to = "CTR", names_repair = "unique"))
  names(defaultAovData) <- c("Day", "AdPlacement", "CTR")
  data(defaultAovData)
  
  updateSelectInput(session, "responseVar", choices = colnames(defaultAovData))
  updateSelectInput(session, "factorVar", choices = colnames(defaultAovData))
  
  observeEvent(input$file, {
    req(input$file)
    data(read.csv(input$file$datapath))
    updateSelectInput(session, "responseVar", choices = colnames(data()))
    updateSelectInput(session, "factorVar", choices = colnames(data()))
  })
  
  observeEvent(input$loadData, {
    req(input$pasteData)
    data(read.table(text = input$pasteData, header = TRUE, sep = "\t", stringsAsFactors = FALSE))
    updateSelectInput(session, "responseVar", choices = colnames(data()))
    updateSelectInput(session, "factorVar", choices = colnames(data()))
  })
  
  observeEvent(input$runANOVA, {
    req(data(), input$factorVar, input$responseVar)
    
    # Check if the selected variables are valid
    if (!all(c(input$factorVar, input$responseVar) %in% colnames(data()))) {
      cat("Error: Selected variables are not valid.")
      return(NULL)
    }
    
    # Check for missing or infinite values in the selected variables
    if (any(is.na(data()[[input$factorVar]])) || any(is.infinite(data()[[input$factorVar]])) ||
        any(is.na(data()[[input$responseVar]])) || any(is.infinite(data()[[input$responseVar]]))) {
      cat("Error: Missing or infinite values detected in the selected variables.")
      return(NULL)
    }
    
    # Check if there is enough variation in the response variable
    if (length(unique(data()[[input$responseVar]])) <= 1) {
      cat("Error: Insufficient variation in the response variable for ANOVA.")
      return(NULL)
    }
    
    # Perform ANOVA
    formula <- as.formula(paste(input$responseVar, "~", input$factorVar))
    anovaResult <- tryCatch(
      aov(formula, data = data()),
      error = function(e) {
        cat("Error: Unable to run ANOVA. Check the data and try again.")
        return(NULL)
      }
    )
    
    # Check if ANOVA was successful
    if (is.null(anovaResult)) {
      return(NULL)
    }
    
    # Save the ANOVA model and factor levels for Tukey's HSD
    anovaModel(anovaResult)
    factorLevels(levels(data()[[input$factorVar]]))
    
    # Display ANOVA summary
    output$anovaSummary <- renderPrint({
      summary(anovaResult)
    })
    
    # Assumptions tests
    assumptionsText <- character()
    
    # Check residuals for normality
    lillieTestResult <- lillie.test(residuals(anovaResult))
    if (lillieTestResult$p.value >= 0.05) {
      assumptionsText <- c(assumptionsText, paste("Lilliefors p-value for residuals =", lillieTestResult$p.value, "Residuals are normally distributed"))
    } else {
      assumptionsText <- c(assumptionsText, paste("Lilliefors p-value for residuals =", lillieTestResult$p.value, "Residuals are not normally distributed"))
    }
    
    # Check residuals for homoscedasticity
    bpTestResult <- bptest(anovaResult, studentize = TRUE)
    if (bpTestResult$p.value >= 0.05) {
      assumptionsText <- c(assumptionsText, paste("Breusch Pagan test p-value for residuals =", bpTestResult$p.value, "Residuals exhibit homoscedasticity"))
    } else {
      assumptionsText <- c(assumptionsText, paste("Breusch Pagan test p-value for residuals =", bpTestResult$p.value, "Residuals exhibit heteroscedasticity"))
    }
    
    # Display assumptions summary
    output$assumptionsSummary <- renderPrint({
      cat(assumptionsText, sep = "\n")
    })
    
    # Conduct Tukey's HSD test
    tryCatch(
      {
        tukeyResult(TukeyHSD(anovaResult))
        
        # Display Tukey's HSD summary
        output$tukeySummary <- renderPrint({
          cat("Tukey's HSD Test:\n")
          print(tukeyResult())
        })
      },
      error = function(e) {
        cat("Error: Unable to run Tukey's HSD. Check the ANOVA model and try again.\n")
      }
    )
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    req(data(), input$factorVar, input$responseVar)
    
    # Check if the selected variables are valid
    if (!all(c(input$factorVar, input$responseVar) %in% colnames(data()))) {
      cat("Error: Selected variables are not valid.")
      return(NULL)
    }
    
    # Create boxplot
    if (is.factor(data()[[input$factorVar]])) {
      # For a categorical factor, create multiple boxplots (facet_wrap)
      ggplot(data(), aes(x = data()[[input$factorVar]], y = data()[[input$responseVar]])) +
        geom_boxplot() +
        labs(title = "Boxplot", x = input$factorVar, y = input$responseVar) +
        facet_wrap(~data()[[input$factorVar]])
    } else {
      # For a non-categorical factor, create a single boxplot
      ggplot(data(), aes(x = 1, y = data()[[input$responseVar]])) +
        geom_boxplot() +
        labs(title = "Boxplot", x = input$factorVar, y = input$responseVar)
    }
  })
  
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
}

# Run the app
shinyApp(ui, server)