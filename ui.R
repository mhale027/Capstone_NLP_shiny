shinyUI(fluidPage(
      
      titlePanel("Data Science Capstone N-Gram Prediction Model"),
      
      sidebarLayout(
            sidebarPanel(
                  helpText("Enter a few words in the text field and the model
                           will predict the word that may follow it, and give 
                           you a list of other choices.")
                  ),
            mainPanel(
                  textInput("input.string", label = h3("Input"), value = "Give it a"),
                  h4("input"),
                  verbatimTextOutput("clean"),
                  br(),
                  br(),
                  actionButton("go", "Predict!"),
                  h4("Main choice"),
                  verbatimTextOutput("my.prediction"),
                  h4("Other choices"),
                  verbatimTextOutput("other.choices")
            )
      )
)
)