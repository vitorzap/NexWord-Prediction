fluidPage(
  # Application title  
  titlePanel("Next word prediction"),

  fluidRow(
     column(12,textInput('txt', label=h3('Text input'),value = '',width ="100%"))
  ),
  fluidRow(
    column(1,actionButton("goButton", "Predict"))  
  ),
  fluidRow(
    column(12,uiOutput("nextWordTitle"))  
  ),
  fluidRow(
     column(12,verbatimTextOutput("nextWord"))  
   ),
  fluidRow(
    column(12,uiOutput("optionsTitle"))  
  ),
  fluidRow(
    column(12,verbatimTextOutput("options"))  
  ),
  tags$br(),tags$br(),
  fluidRow(column(12,p(a(href='https://github.com/vitorzap/NexWord-Prediction/tree/master', 'See the source -> github'),style="text-align:right")))
)