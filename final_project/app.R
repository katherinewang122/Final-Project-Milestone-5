library(shiny)
library(shinythemes)
library(readr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(tidycensus)
library(readxl)
library(janitor)
library(ggmap)
library(maps)
library(mapdata)
library(viridis)
library(gganimate)
library(gifski)
library(plotly)

# this is for constructing the poverty status by sex graph
datapoints <- c("geo_name", "se_a12003a_001", "se_a12003a_002", 
                "se_a12003a_003", "se_a12003b_001", "se_a12003b_002", 
                "se_a12003b_003", "se_a14009_001", "se_a14009_002", 
                "se_a14009_003", "se_a14009_004", "se_a14009_005", 
                "se_a14009_006", "se_a14009_009", "se_a14009_010")

datapoints1 <- c("se_a12003a_001", "se_a12003a_002", "se_a12003a_003", 
                 "se_a12003b_001", "se_a12003b_002", "se_a12003b_003", 
                 "se_a14009_001", "se_a14009_002", "se_a14009_003", 
                 "se_a14009_004", "se_a14009_005", "se_a14009_006", 
                 "se_a14009_009", "se_a14009_010")

# this is for constructing the poverty status by race plot
datapoints2 <- c("geo_name", "se_a13001a_001", "se_a13001a_002", 
                 "se_a13001b_001", "se_a13001b_002", "se_a13001d_001", 
                 "se_a13001d_002", "se_a13001h_001", "se_a13001h_002", 
                 "se_a13005_001", "se_a13005_003", "se_a13005_004")

datapoints3 <- c("geo_name", "se_a17005a_001", "se_a17005a_003", 
                 "se_a17005b_001", "se_a17005b_003", "se_a17006a_001", 
                 "se_a17006a_003", "se_a17006b_001", "se_a17006b_003", 
                 "se_a17006d_001", "se_a17006d_003", "se_a17006h_001", 
                 "se_a17006h_003")

datapoints4 <- c("geo_name", "se_a12003a_001", "se_a12003a_002", 
                 "se_a12003b_001", "se_a12003b_002", "se_a17005a_001", 
                 "se_a17005a_003", "se_a17005b_001", "se_a17005b_003", 
                 "se_a17006a_001", "se_a17006a_003", "se_a17006b_001", 
                 "se_a17006b_003", "se_a17006d_001", "se_a17006d_003", 
                 "se_a17006h_001", "se_a17006h_003")

us_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
               "Colorado", "Connecticut", "Delaware", "District of Columbia", 
               "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
               "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
               "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
               "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
               "New Jersey", "New Mexico", "New York", "North Carolina", 
               "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
               "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
               "Texas", "Utah", "Vermont", "Virginia", "Washington", 
               "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")

# I downloaded the ACS file, transformed all the columns into character values
# so that it could be loaded into a table, and skipped the first line so that
# the first row of the tables shows the variable names. I cleaned the names in
# order to facilitate calling variables later on, and I selected certain
# variables to display, which were collectively under the list called
# datapoints.

acs2017 <- read_csv("R12497278_SL040 copy.csv", 
                    col_types = cols(.default = col_character()), 
                    skip = 1) %>% 
    clean_names() %>% 
    select(all_of(datapoints))

# In order to convert the columns from characters into numeric values, I used
# the as.numeric function on the entire tibble. However, this rendered the first
# column, which showed the names of the states, into NAs. Thus, I first pulled
# out the state names column, changed the rest of the tibble into numeric
# values, created a new list of all the states names, and then joined that list
# with the numeric tibble to get a table that has state names as characters and
# all the numbers as numeric values.

acs_states <- acs2017[1]
acs2017[] <- lapply(acs2017, function(x) as.numeric(as.character(x)))
acs_a <- acs2017 %>% select(datapoints1) %>% mutate(geo_name = us_states)
acs <- inner_join(acs_states, acs_a, by = "geo_name")

# I also loaded the NCES data, skipped the first five lines so that the variable
# names are in the first row, and selected the variables I needed. I omitted all
# the NAs to make the data more readable.

nces2017 <- read_xls("tabn219.46 copy.xls", skip = 5) %>% 
    select("1", "9", "10", "11", "12") %>% 
    na.omit()

# this is for constructing the poverty status by race plot
pov_status <- read_csv("R12518258_SL040 copy.csv") %>% 
    clean_names() %>% 
    select(all_of(datapoints2))
pov_status$geo_name <- tolower(pov_status$geo_name)

# unemployment data
unemp_data <- read_csv("R12529099_SL040 copy.csv") %>% 
    clean_names() %>% 
    select(all_of(datapoints3))

# county data
county_data <- read_csv("R12536075_SL050 copy.csv") %>% 
    clean_names() %>% 
    select(all_of(datapoints4)) %>% 
    mutate(subregion = sapply(strsplit(county_data$geo_name, " County"), "[", 1)) %>% 
    mutate(region = sapply(strsplit(county_data$geo_name, ", "), "[", -1))
county_data$subregion <- tolower(county_data$subregion)
county_data$region <- tolower(county_data$region)

# I downloaded data for the states, which is already stored in R. 

states <- map_data("state")
county <- map_data("county")

# Define UI for application that shows graphs
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    navbarPage(tags$b("Schools as Social Mirrors"),
    
    tabPanel("Findings",
             titlePanel("Findings"),
             mainPanel(
                 imageOutput("combinedrace1"),
                 br(), br(), br(), br(), br(), br(), br(),
                 imageOutput("combinedsex1"),
                 br(), br(), br(), br(), br(), br(), br(),
                 br(), br(), br(), br(), br(), br(), br(),
                 plotOutput("regressionplot"),
                 br(), br(), br(), br(),
                 plotOutput("regressionplot1")
             )
             ),
    
    tabPanel("In-Depth Analysis of Gender Inequality",
             titlePanel("In-Depth Analysis of Gender Inequality"),
             sidebarPanel(
                 selectInput("state", "State: ",
                             c("Alabama" = "ALABAMA", # no alaska
                               "Arizona" = "ALABAMA", 
                               "Arkansas" = "ARKANSAS", "California" = "CALIFORNIA", 
                               "Colorado" = "COLORADO", 
                               "Connecticut" = "CONNECTICUT", 
                               "Delaware" = "DELAWARE", "Florida" = "FLORIDA", 
                               "Georgia" = "GEORGIA", # no hawaii
                               "Idaho" = "IDAHO", "Illinois" = "ILLINOIS", 
                               "Indiana" = "INDIANA", "Iowa" = "IOWA", 
                               "Kansas" = "KANSAS", "Kentucky" = "KENTUCKY", 
                               "Louisiana" = "LUOISIANA", "Maine" = "MAINE", 
                               "Maryland" = "MARYLAND", "Massachusetts" = "MASSACHUSETTS", 
                               "Michigan" = "MICHIGAN", "Minnesota" = "MINNESOTA", 
                               "Mississippi" = "MISSISSIPPI", "Missouri" = "MISSOURI",
                               "Montana" = "MONTANA", "Nebraska" = "NEBRASKA", 
                               "Nevada" = "NEVADA", "New Hampshire" = "NEW HAMPSHIRE", 
                               "New Jersey" = "NEW JERSEY", "New Mexico" = "NEW MEXICO", 
                               "New York" = "NEW YORK", "North Carolina" = "NORTH CAROLINA", 
                               "North Dakota" = "NORTH DAKOTA", "Ohio" = "OHIO", 
                               "Oklahoma" = "OKLAHOMA", "Oregon" = "OREGON", 
                               "Pennsylvania" = "PENNSYLVANIA", "Rhode Island" = "RHODE ISLAND", 
                               "South Carolina" = "SOUTH CAROLINA", 
                               "South Dakota" = "SOUTH DAKOTA", "Tennessee" = "TENNESSEE", 
                               "Texas" = "TEXAS", "Utah" = "UTAH", 
                               "Vermont" = "VERMONT", "Virginia" = "VIRGINIA", 
                               "Washington" = "WASHINGTON", "West Virginia" = "WEST VIRGINIA", 
                               "Wisconsin" = "WISCONSIN", "Wyoming" = "WYOMING")),
                 selectInput("variable", "Variable: ",
                             c("School Dropout Rate" = "Dropout Rate",
                               "Unemployment Rate" = "Unemployment Rate"))
                 ),
             mainPanel(
                 plotOutput("county_plot"),
                 br(), br(),
                 plotOutput("county_regression")
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
             measures a school's racial inequality."),
             h3("About Me"),
             p("My name is Katherine, and I plan to concentrate in social studies
             with a secondary in economics.
             You can reach me at katherinewang1@college.harvard.edu.")))
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$combinedrace1 <- renderImage({
        list(src = "combined_race.gif",
             alt = "combined_race.gif",
             width = 650,
             height = 600)}, 
            deleteFile = FALSE)
    
    output$combinedsex1 <- renderImage({
        list(src = "combined_sex.gif",
             alt = "combined_sex.gif",
             width = 700,
             height = 700)}, 
        deleteFile = FALSE)
    
    output$regressionplot <- renderPlot({
        combined_unemp_race <- combined_unemp_race
        combined_unemp_race_reduced <- unique( combined_unemp_race[ , 1:3 ] )
        combined_final1 <- combined_final1 %>% 
            select(geo_name, drop_rate, race)
        combined_final1_reduced <- unique( combined_final1[ , 1:3 ] )
        combined_new <- left_join(combined_unemp_race_reduced, combined_final1_reduced, 
                                  by = c("geo_name","race"))
        
        ggplot(combined_new, aes(x = unemp, y = drop_rate, color = race)) +
            geom_point() +
            geom_smooth(method = "lm", se = F) +
            labs(title = "Relationship Between Unemployment Rate and School Dropout Rate \n Based on Race in 2017",
                 color = "Race",
                 x = "Unemployment Rate",
                 y = "School Dropout Rate \n (For Population 16-19 Years)",
                 caption = "Source: American Community Survey 2017 and National Center for Education Statistics 2017") +
            scale_x_continuous(breaks = c(5, 10, 15), labels = c("5%", "10%", "15%")) +
            scale_y_continuous(breaks = c(10, 20, 30), labels = c("10%", "20%", "30%")) +
            scale_color_manual(values = c("goldenrod2", "darkolivegreen", "deeppink2", "midnightblue"), 
                               labels = c("Asian", "Black", "Hispanic", "White")) +
            theme_classic()
    })
    
    output$regressionplot1 <- renderPlot({
        both <- both %>% select(region, drop_rate, type)
        joined_sex <- both1 %>% left_join(both, by = c("region", "type"))
        
        ggplot(joined_sex, aes(x = unemp, y = drop_rate, color = type)) +
            geom_point() +
            geom_smooth(method = "lm", se = F) +
            labs(title = "Relationship Between Unemployment Rate and School Dropout Rate \n Based on Sex in 2017",
                 color = "Sex",
                 x = "Unemployment Rate",
                 y = "School Dropout Rate \n (For Population 16-19 Years)",
                 caption = "Source: American Community Survey 2017 and National Center for Education Statistics 2017") +
            scale_x_continuous(breaks = c(5, 10, 15), labels = c("5%", "10%", "15%")) +
            scale_y_continuous(breaks = c(2, 3, 4, 5, 6, 7), labels = c("2%", "3%", "4%", "5%", "6%", "7%")) +
            scale_color_manual(values = c("deeppink", "deepskyblue"), 
                               labels = c("Female", "Male")) +
            theme_classic()
    })
    
    output$county_plot <- renderPlot({
        
        if(input$state == "") {
            return(NULL)
        }
        
        dropout_county_male <- county_data %>% 
            select(region, subregion, se_a12003a_001, se_a12003a_002) %>% 
            mutate(data = (se_a12003a_002/se_a12003a_001)*100) %>% 
            mutate(type = "male") %>% 
            mutate(datatype = "Dropout Rate") %>% 
            select(region, subregion, data, type, datatype)
        
        dropout_county_female <- county_data %>% 
            select(region, subregion, se_a12003b_001, se_a12003b_002) %>% 
            mutate(data = (se_a12003b_002/se_a12003b_001)*100) %>% 
            mutate(type = "female") %>% 
            mutate(datatype = "Dropout Rate") %>% 
            select(region, subregion, data, type, datatype)
        
        dropout_county_both <- rbind(dropout_county_male, dropout_county_female)
        
        unemp_county_male <- county_data %>% 
            select(region, subregion, se_a17005a_001, se_a17005a_003) %>% 
            mutate(data = (se_a17005a_003/se_a17005a_001)*100) %>% 
            mutate(type = "male") %>% 
            mutate(datatype = "Unemployment Rate") %>% 
            select(region, subregion, data, type, datatype)
        
        unemp_county_female <- county_data %>% 
            select(region, subregion, se_a17005b_001, se_a17005b_003) %>% 
            mutate(data = (se_a17005b_003/se_a17005b_001)*100) %>% 
            mutate(type = "female") %>% 
            mutate(datatype = "Unemployment Rate") %>% 
            select(region, subregion, data, type, datatype)
        
        unemp_county_both <- rbind(unemp_county_male, unemp_county_female)
        county_all <- rbind(unemp_county_both, dropout_county_both)
        county_all_final <- left_join(county, county_all, by = c("region", "subregion"))
        
        county_all_final$type <- toupper(county_all_final$type)
        county_all_final$region <- toupper(county_all_final$region)
        
        county_all_final %>% 
            filter(region == input$state) %>%
            filter(datatype == input$variable) %>% 
            ggplot(aes(x = long, y = lat, group = group, fill = data)) +
            geom_polygon(color = "white") +
            facet_wrap(~ type) +
            labs(title = paste(input$state, "| Map of", input$variable, "by Sex in 2017"),
                 subtitle = paste("County Level Data"),
                 caption = "Source: American Community Survey 2017",
                 fill = input$variable) +
            scale_fill_viridis(option = "magma", 
                               direction = -1,
                               guide = guide_colorbar(direction = "horizontal",
                                                      barheight = unit(2, units = "mm"),
                                                      barwidth = unit(35, units = "mm"),
                                                      draw.ulim = FALSE,
                                                      title.position = "top",
                                                      title.hjust = 0.5,
                                                      label.hjust = 0.5)) +
            theme(legend.position = "bottom",
                  legend.spacing.x = unit(1, 'cm')) +
            theme_void()
        })
    
    output$county_regression <- renderPlot({
        
        if(input$state == "") {
            return(NULL)
        }
        
        dataset1 <- county_all_final %>% 
            filter(region == input$state) %>% 
            filter(datatype == "Dropout Rate") %>% 
            mutate(dropout1 = data) %>% 
            select(region, subregion, dropout1, type)
        
        dataset1_reduced <- unique( dataset1[ , 1:4 ] )
        
        dataset2 <- county_all_final %>% 
            filter(region == input$state) %>% 
            filter(datatype == "Unemployment Rate") %>% 
            mutate(unemp1 = data) %>% 
            select(region, subregion, unemp1, type)
        
        dataset2_reduced <- unique( dataset2[ , 1:4 ] )
        
        county_reg_joined <- dataset1_reduced %>% 
            left_join(dataset2_reduced, by = c("region", "subregion", "type"))
        
        county_reg_joined %>% 
            ggplot(aes(x = unemp1, y = dropout1, color = type)) +
            geom_point() +
            geom_smooth(method = "lm", se = F) +
            labs(title = paste(input$state, 
                               "| Relationship between School Dropout Rate and Unemployment Rate by Sex in 2017"),
                 subtitle = paste("County Level Data"),
                 caption = "Source: American Community Survey 2017",
                 color = "Sex",
                 x = "Unemployment Rate",
                 y = "School Dropout Rate \n (For Population 16-19 Years)") +
            scale_x_continuous(breaks = c(2.5, 5.0, 7.5, 10.0, 12.5), 
                               labels = c("2.5%", "5.0%", "7.5%", "10.0%", "12.5%")) +
            scale_y_continuous(breaks = c(0, 10, 20, 30), 
                               labels = c("0%", "10%", "20%", "30%")) +
            scale_color_manual(values = c("deeppink", "deepskyblue")) +
            theme_classic()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
