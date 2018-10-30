library(shiny)
require(dismo)
library(kernlab)
library(bartMachine)

load("BARTModel.rda")


ui <- fluidPage(
  fluidRow(
    column(width = 2,
           tags$br(),
           img(src='heartlink_logo.png', style="width:200px"))
  ),
  helpText("Prediction of likelihood of heart disease presence based on
            UCI heart disease dataset using 
           Bayesian additive regression trees."),
  wellPanel (fluidRow(
    column(width = 4,
           sliderInput(inputId = "age",
                       label = "Age",
                       value = 25, min = 1, max = 100),
           radioButtons(inputId = "sex", 
                        label = "Sex",
                        choices = c("Male" = 1,
                                    "Female" = 0)),
           radioButtons(inputId = "cp",
                        label = "Chest Pain Type",
                        choices = c("Typical Angina" = 1,
                                    "Atypical Angina" = 2,
                                    "Non-anginal Pain" = 3,
                                    "Asymptomatic" = 4)),
           sliderInput(inputId = "trestbps",
                       label = "Resting Systolic Blood Pressure",
                       value = 120, min = 90, max = 250)),
    column(width = 4,
           sliderInput(inputId = "chol",
                       label = "Serum Cholesterol (mg/dl)", 
                       value = 150, min = 0, max = 500),
           sliderInput(inputId = "fbs",
                       label = "Fasting Blood Sugar (mg/dl)",
                       value = 120, min = 0, max = 500),
           radioButtons(inputId = "restecg", 
                        label = "Resting ECG Results",
                        choices = c("Normal" = 0,
                                    "Having ST-T wave abnormality" = 1,
                                    "Showing probable or definite left ventricular hypertrophy by Estes' criteria" = 2)),
           sliderInput(inputId = "thalach",
                       label = "Maximum Heart Rate Achieved",
                       value = 120, min = 60, max = 220)
    ),
    column(width = 4,
           radioButtons(inputId = "exang",
                        label = "Exercise Induced Angina",
                        choices = c("Yes" = 1,
                                    "No" = 0)),
           sliderInput(inputId = "oldpeak",
                       label = "ST Depression Induced by Exercise Relative To Rest",
                       value = 0, min = -10, max = 10, step = 0.1),
           radioButtons(inputId = "slope",
                        label = "The Slope of the Peak Exercise ST Segment",
                        choices = c("Upsloping" = 1,
                                    "Flat" = 2,
                                    "Downsloping" = 3)),
           radioButtons(inputId = "thal",
                        label = "Thallium Stress Test Result",
                        choices = c("Normal" = 3,
                                    "Fixed Defect" = 6,
                                    "Reversable Defect" = 7)))
  )),
  fluidRow(
    column(width = 12,
           actionButton(inputId = "analyse",
                        label = "Analyse",
                        width = "100%",
                        icon = icon(name = "heartbeat", lib = "font-awesome")))
  ),
  fluidRow(tags$br(),
          htmlOutput("text"))
)

server <- function(input, output) {
  data <- eventReactive(input$analyse, {
    fbs <- 0
    if(input$fbs > 120) {
      fbs <- 1
    }
    
    #Prepare dataframe 
    df = data.frame("age" = as.integer(input$age), 
                    "sex" = as.factor(input$sex), 
                    "cp" = as.factor(input$cp),
                    "trestbps" = as.integer(input$trestbps),
                    "chol" = as.integer(input$chol),
                    "fbs" = as.factor(fbs),
                    "restecg" = as.factor(input$restecg),
                    "thalach" = as.integer(input$thalach),
                    "exang" = as.factor(input$exang),
                    "oldpeak" = as.double(input$oldpeak),
                    "slope" = as.factor(input$slope),
                    "thal" = as.factor(input$thal))
    prediction <- predict(model, df, type="class")
    table <- table(prediction)
    table
  }) 
  output$text <- renderText({
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Analysing', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    if(as.integer(data()[1]) == 1){
      print("<div class=\"col-sm-12\"><div class=\"well\" style=\"background-color:#D8F1D8;\"><font color=\"#00A300\"><b>Unlikely presence of heart disease</b></font></div></div>")
    } else if (as.integer(data()[2] == 1)) {
      print("<div class=\"col-sm-12\"><div class=\"well\" style=\"background-color:#ffcccc;\"><font color=\"#e50000\"><b>Likely presence of heart disease</b></font></div></div>")
    } else {
      print("Error, please try reinserting new data.")
    }
  })
}

shinyApp(server = server, ui = ui)