library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Final Project"),

    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("image")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$image <- renderImage({
        list(src = "gradrace_plot.png",
             alt = "gradrace_plot.png")
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
