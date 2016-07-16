shinyUI(fluidPage(
    
    textInput("text", label = h3("Text input"), value = "Enter text..."),
    numericInput("number", label = h3("Number of suggestions"), value = 5),
    submitButton("Submit!"),
    hr(),
    fluidRow(column(4, verbatimTextOutput("value")))
    
))