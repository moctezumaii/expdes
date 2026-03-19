# download the shiny package in order to run this in your computer

library(shiny)

# Define UI for application that draws a histogram
shinyApp(

  ui = fluidPage(
    titlePanel("Lambda"),
    mainPanel(
      column(4, wellPanel(
        sliderInput("lambda", "lambda Blue Distribution:",
                    min = 0, max = 50, value = 5, step = 2)
      ),


      wellPanel(
        checkboxInput("header", "Two graphs", TRUE)
      ),

      wellPanel(
        sliderInput("lambda2", "lambda Blue Distribution:",
                    min = 0, max = 50, value = 7, step = 2)
      )
      ),
      column(8,
             plotOutput('curves'),

             br(),
             br()

      )

    )
  ),

  server = function(input, output, session) {
    # Create a random name for the log file




    # This observer adds an entry to the log file every time
    # input$n changes.
    x <- 0:80
    maxvalue <- reactive({

      max(c(dpois(x, input$lambda),dpois(x, input$lambda2)))
    })

    #    redvalsm<-reactive({
    #      if(input$header==1){
    #        input$mean2
    #        } else NA
    #    })

    output$curves <- renderPlot({
      plot(NULL, xlim=c(0,80), ylim=c(0, maxvalue()),  ylab = 'Probability', xlab = 'X',
           main = "Two Poisson Distributions" )
      barplot(dpois(x, input$lambda),
            col = alpha('steelblue',0.6),add=T)
      if(input$header==1){
        barplot(dpois(x, input$lambda2),
                col = alpha('tomato',0.6),add=T)
      }

    })


  }
)
