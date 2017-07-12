shinyServer( function(input, output) {
           
            output$clean <- eventReactive( input$input.string, { clean.input( input$input.string ) })
            
            output$my.prediction <- eventReactive( input$go, { pk( clean.input( input$input.string ) ) })
            
            output$other.choices <- eventReactive(input$go, { get_choices( clean.input(input$input.string) ) })
            
      }     
)