library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", # how to internally refer to this particular input
              label = "Choose a number", # label for the slider
              value = 25, min = 1, max = 100), # additional arguments
  plotOutput("hist") # how to internally refer to this parameter; creates an output list with this parameter
)

server <- function(input, output) {
  # you can have multiple input and output objects
  # in case you have multiple output objects and you want to refer to the same input object; you do this with reactive({}) and store it in an object
        # when you refer to it later, you treat it as a function so add parentheses at the end
  # here we can refer to the input/output values with $
  output$hist <- renderPlot({ # you need the render function for reactivity; and {} to specify code
    hist(rnorm(input$num)) # with the render functions, you specify how the outputs are BUILT
    # importantly, render() functions will rerun every time one of the input objects changes!
  })
}

shinyApp(ui = ui, server = server)
