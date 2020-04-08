library(shiny)
library(shinythemes)

# Define UI for application that shows graphs
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    navbarPage(tags$b("School Inequality in America"),
    
    tabPanel("Findings",
             titlePanel("Findings"),
             plotOutput("gradrace"),
             plotOutput("gradsex")
             ),
    
    tabPanel("In-Depth Analysis",
             titlePanel("In-Depth Analysis"),
             mainPanel(
                 p("Tour of the modeling choices you made and 
                 an explanation of why you made them")
             )
             ),
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("The goal of this project is to discover the extent to which a 
             school’s inequality mirrors that of its community’s. Although some 
             believe that schools act as the springboard of social equality and 
             progress, others argue that schools merely reinforce the existing 
             stratification found in their societies. In order to test the idea 
             of whether schools serve as social mirrors, I will analyze two main
             forms of inequality, race and gender, in two separate spheres -- 
             the school and the community. Ultimately, I hope to discover 
             whether a correlative relationship exists between inequality in a 
             community and inequality in schools."),
             p("I gathered my data from three sources: the American Community 
             Surveys (ACS) from the US Census Bureau, the National Center 
             for Education Statistics (NCES), and the Status of Women in the 
             States. From ACS’ data, I extracted information about each 
             state's school dropout rates by gender and  average household 
             income by race. The dropout rates correspond to a school's 
             gender inequality, whereas the household income by race measures 
             a community's racial inequality. From the data from the National 
             Center for Education Statistics (NCES), I retrieved the 
             adjusted cohort graduation rate (ACGR) based on race, which 
             measures a school's racial inequality. Finally, from the Status 
             of Women in the States, I obtained data on the gender earnings 
             ratio among the states, which measures a community's gender 
             inequality."),
             h3("About Me"),
             p("My name is Katherine, and I plan to concentrate in social studies.
             You can reach me at katherinewang1@college.harvard.edu.")))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$gradrace <- renderImage({
        list(src = "gradrace_plot.png",
             alt = "gradrace_plot.png",
             height = 400,
             width = 550)
    }, deleteFile = FALSE)
    
    output$gradsex <- renderImage({
        list(src = "gradsex_plot.png",
             alt = "gradsex_plot.png",
             height = 700,
             width = 700)
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
