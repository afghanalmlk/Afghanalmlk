# Load the required libraries
library(shiny)
library(DT)
library(dplyr)
library(lmtest)
library(nortest)
library(corrplot)

# Define the UI
ui <- fluidPage(
  titlePanel("Regression App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      textAreaInput("pasteData", "Paste Data from Clipboard", rows = 10),
      actionButton("loadData", "Load Data"),
      actionButton("displayCorrelationMatrix", "Display Correlation Matrix"),
      
      # Regression options
      sliderInput("splitValue", "Train/Test Splitting Percentage", min = 50, max = 100, value = 70),
      selectInput("responseVar", "Select Response Variable", ""),
      selectInput("predictorVars", "Select Predictor Variables", "", multiple = TRUE),
      actionButton("runRegression", "Run Multiple Linear Regression"),
      
      # Prediction options
      actionButton("runPredictions", "Run Model Predictions for Test Data"),
      
      # Input values for making predictions
      uiOutput("inputPredictionValues"),
      actionButton("makePredictions", "Make Predictions")
    ),
    mainPanel(
      DTOutput("dataTable"),
      verbatimTextOutput("regressionSummary"),
      verbatimTextOutput("assumptionsSummary"),
      verbatimTextOutput("predictionOutput"),
      plotOutput("correlationPlot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  regressionModel <- reactiveVal(NULL)
  testData <- reactiveVal(NULL)
  correlationMatrix <- reactiveVal(NULL)
  
  # Set the default dataset
  defaultData <- read.csv("data//business.csv", sep = ";")
  data(defaultData)
  
  updateSelectInput(session, "responseVar", choices = colnames(defaultData))
  updateSelectInput(session, "predictorVars", choices = colnames(defaultData))
  
  observeEvent(input$file, {
    req(input$file)
    data(read.csv(input$file$datapath))
    updateSelectInput(session, "responseVar", choices = colnames(data()))
    updateSelectInput(session, "predictorVars", choices = colnames(data()))
  })
  
  observeEvent(input$loadData, {
    req(input$pasteData)
    data(read.table(text = input$pasteData, header = TRUE, sep = "\t", stringsAsFactors = FALSE))
    updateSelectInput(session, "responseVar", choices = colnames(data()))
    updateSelectInput(session, "predictorVars", choices = colnames(data()))
  })
  
  observeEvent(input$displayCorrelationMatrix, {
    req(data())
    
    # Select only numeric columns for correlation calculation
    numericData <- data() %>% select_if(is.numeric)
    
    # Calculate the correlation matrix
    corrMatrix <- round(cor(numericData), 2)
    
    # Save the correlation matrix for later use
    correlationMatrix(corrMatrix)
    
    # Display correlation plot
    output$correlationPlot <- renderPlot({
      corrplot(corrMatrix, method = 'number', type='lower')
    })
  })
  
  observeEvent(input$runRegression, {
    req(data())
    
    # Check if the requested split size is smaller than the number of rows in the dataset
    if (nrow(data()) * input$splitValue / 100 < 2) {
      cat("Error: Cannot perform split with the specified percentage. Choose a smaller percentage.")
      return(NULL)
    }
    
    # Train/test splitting
    set.seed(123)
    trainIndices <- sample(1:nrow(data()), round(nrow(data()) * input$splitValue / 100))
    trainData <- data()[trainIndices, ]
    testData <- data()[-trainIndices, ]
    
    # Check if at least one predictor variable is selected
    if (length(input$predictorVars) == 0) {
      cat("Error: Select at least one predictor variable.")
      return(NULL)
    }
    
    # Perform multiple linear regression
    formula <- as.formula(paste(input$responseVar, "~", paste(input$predictorVars, collapse = "+")))
    lmModel <- lm(formula, data = trainData)
    
    # Save the model and test data for predictions
    regressionModel(lmModel)
    testData(testData)
    
    # Display regression summary
    output$regressionSummary <- renderPrint({
      summary(lmModel)
    })
    
    # Assumptions tests
    assumptionsText <- character()
    
    # Durbin-Watson test for autocorrelation
    dwTestResult <- dwtest(lmModel)
    if (dwTestResult$p.value >= 0.05) {
      assumptionsText <- c(assumptionsText, paste("The p-value for the Durbin-Watson test in this model is", dwTestResult$p.value, ". There is no autocorrelation in this model."))
    } else {
      assumptionsText <- c(assumptionsText, paste("The p-value for the Durbin-Watson test in this model is", dwTestResult$p.value, ". There is autocorrelation in this model."))
    }
    
    # Blank line
    assumptionsText <- c(assumptionsText, "")
    
    # Breusch-Pagan test for homoskedasticity
    bpTestResult <- bptest(lmModel, studentize = TRUE)
    if (bpTestResult$p.value >= 0.05) {
      assumptionsText <- c(assumptionsText, paste("The p-value for the Breusch Pagan test in this model is", bpTestResult$p.value, ". Homoscedasticity assumptions are accomplished."))
    } else {
      assumptionsText <- c(assumptionsText, paste("The p-value for the Breusch Pagan test in this model is", bpTestResult$p.value, ". There is heteroscedasticity in this model."))
    }
    
    # Blank line
    assumptionsText <- c(assumptionsText, "")
    
    # Lilliefors test for normality of residuals
    lillieTestResult <- lillie.test(lmModel$residuals)
    if (lillieTestResult$p.value >= 0.05) {
      assumptionsText <- c(assumptionsText, paste("The p-value for the Lilliefors test in this model is", lillieTestResult$p.value, ". The residuals are normally distributed."))
    } else {
      assumptionsText <- c(assumptionsText, paste("The p-value for the Lilliefors test in this model is", lillieTestResult$p.value, ". The residuals are not normally distributed."))
    }
    
    # Blank line
    assumptionsText <- c(assumptionsText, "")
    
    # Variance Inflation Factor (VIF) for multicollinearity
    if (length(input$predictorVars) > 1) {
      vifValues <- car::vif(lmModel)
      for (i in seq_along(vifValues)) {
        if (vifValues[i] >= 10) {
          assumptionsText <- c(assumptionsText, paste("The VIF for", names(vifValues)[i], "is", vifValues[i], ". Potential multicollinearity is detected."))
        } else {
          assumptionsText <- c(assumptionsText, paste("The VIF for", names(vifValues)[i], "is", vifValues[i], "."))
        }
      }
    } else {
      assumptionsText <- c(assumptionsText, "Not enough predictor variables for VIF calculation.")
    }
    
    # Display assumptions summary
    output$assumptionsSummary <- renderPrint({
      cat(assumptionsText, sep = "\n")
    })
  })
  
  observeEvent(input$runPredictions, {
    req(regressionModel(), testData())
    
    # Predictions for test data
    testPredictions <- predict(regressionModel(), newdata = testData(), interval = "prediction")
    
    # Display prediction output
    output$predictionOutput <- renderPrint({
      cat("Prediction Output for Test Data:\n")
      
      # Extracting predicted values, actual values, and residuals
      predictedValues <- testPredictions[, "fit"]
      actualValues <- testData()[, input$responseVar]
      residuals <- actualValues - predictedValues
      
      # Create a data frame with predicted, actual, and residuals
      predictionData <- data.frame(Predicted = predictedValues, Actual = actualValues, Residuals = residuals)
      
      # Display the data frame
      print(predictionData)
      
      # Calculate and display accuracy score (you might want to define your own metric based on the nature of your response variable)
      accuracy <- mean(residuals^2)
      cat("\nAccuracy Score:", accuracy)
    })
  })
  
  # Dynamic UI for input values for making predictions
  output$inputPredictionValues <- renderUI({
    req(regressionModel())
    
    # Creating numeric input boxes for each selected predictor variable
    lapply(input$predictorVars, function(varName) {
      numericInput(inputId = varName, label = paste("Enter", varName, "Value"), value = 0)
    })
  })
  
  observeEvent(input$makePredictions, {
    req(regressionModel())
    
    # Collecting input values for making predictions
    inputValues <- sapply(input$predictorVars, function(varName) {
      input[[varName]]
    })
    
    # Creating a data frame with input values
    inputDataFrame <- data.frame(t(inputValues))
    colnames(inputDataFrame) <- input$predictorVars
    
    # Making predictions based on the model
    predictions <- predict(regressionModel(), newdata = inputDataFrame, interval = "prediction")
    
    # Display prediction output
    output$predictionOutput <- renderPrint({
      cat("Predictions based on Input Values:\n")
      
      # Extracting predicted values, lower and upper bounds
      predictedValues <- predictions[, "fit"]
      lowerBounds <- predictions[, "lwr"]
      upperBounds <- predictions[, "upr"]
      
      # Creating a data frame with predictions and confidence intervals
      predictionsDataFrame <- data.frame(Predicted = predictedValues, Lower_Bound = lowerBounds, Upper_Bound = upperBounds)
      
      # Display the data frame
      print(predictionsDataFrame)
    })
  })
  
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
}

# Run the application
shinyApp(ui, server)