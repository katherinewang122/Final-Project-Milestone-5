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
    
    tabPanel("Featured Findings",
             h1(tags$b("Schools as Social Mirrors"), 
                align = "center"),
             p(tags$em("An Analysis of School and Societal Inequality"), 
               align = "center"),
             fluidRow(column(2), column(8,
             p("Do schools act as springboards of social progress, or do they 
               merely reinforce existing inequalities? My project attempts to 
               nswer this question by analyzing the separate effects of two 
               types of inequality, race and gender, on schools and communities."),
             p("Hunting for the right variables to analyze was one of my largest 
               concerns throughout this project. Any attempt to quantify inequality 
               risks oversimplification. Thus, the data provided in this project merely 
               outlines certain forms of inequality, and does not account for the 
               lived experiences of those who have been disproportionately impacted by it."),
             p("Finding data for inequality at the school level also proved 
               particularly challenging, as most attempts to measure inequality 
               have been conducted at the national or state level. In the end, I 
               settled on using school dropout rates as an indicator of school 
               inequality because students who drop out typically do so in order 
               to support their family (economic necessity) or because they did not 
               feel a sense of belonging or safety (discrimination) -- both of 
               which are signs of inequality. For measuring societal inequality, 
               which I conducted at both the state and county levels, I analyzed 
               the unemployment rate and poverty status, both of which indicate 
               socioeconomic inequality."),
             p("So does a school’s inequality reflect that of its community’s? 
               The four small maps below (one map for each race) provide an 
               overview of the comparison of the school dropout rate and the 
               unemployment rate. From the maps, we see that Asians and Whites 
               experience similar low levels of school dropout and unemployment rates, 
               whereas Blacks experience the highest levels of both rates. The 
               correlation between the school dropout rate and the unemployment rate 
               seems ambiguous: although some states that have higher unemployment 
               rates also have higher school dropout rates, such as New Mexico and 
               Nevada, other states that have lower unemployment rates have high 
               school dropout rates, such as North Dakota."))),
             fluidRow(column(2), column(8, align = "center",
             imageOutput("combinedrace1", width = "100%", height = "100%"))),
             fluidRow(column(2), column(8,
             p("In order to get a clearer sense of the correlation between the 
               unemployment rate and the school dropout rate, I created a linear 
               regression between the two variables for the different races. The 
               regression shows that there is a positive correlation between the 
               unemployment rate and the school dropout rate for Blacks, Hispanics, 
               and Whites, but a strong negative correlation for Asians. This means 
               that for Blacks, Hispanics, and Whites, a higher unemployment rate 
               is associated with a higher school dropout rate; on the other hand, 
               for Asians, a higher unemployment rate is associated with a lower 
               school dropout rate. However, the regression plots for Asians and 
               Whites might not be that accurate because the data points are more 
               clustered together, so outliers might unduly influence the slope 
               of the regression."),
             p("In addition, the fact that the Hispanic and Black regression plots 
               experience a greater outward shift signify that those two races 
               generally have higher rates for both unemployment and school dropout 
               than the rates for Asians and Whites. This supports what we have 
               observed previously in the four maps."),
             plotOutput("regressionplot1"), br(),
             p("Now that we have analyzed the effects of unemployment on school 
               dropout rates for different races, what about the effects on sex? 
               Overall, male students experience a higher dropout rate than their 
               female counterparts. In some states, we see a correlation between 
               the unemployment rate and the school dropout rate: for instance, 
               Louisiana and New Mexico have both high unemployment and school 
               dropout rates, and Nebraska and Virginia. However, other states 
               like California seem to display contrasting results, as it has a 
               low school dropout rate but a high unemployment rate."))),
             fluidRow(column(2), column(8, align = "center",
             imageOutput("combinedsex1", width = "100%", height = "100%"))),
             fluidRow(column(2), column(8,
             p("Like before, I used a regression to analyze the correlation between 
               the unemployment rate and the school dropout rate based on sex. I 
               removed two outliers that disproportionately influenced the slope 
               of the regressions. From the graph, we notice that there is a 
               positive correlation between the unemployment rate and the school 
               dropout rate for both males and females, but the correlation is 
               stronger for males. This signifies that a higher unemployment rate 
               for males is associated with a higher school dropout rate for male 
               students, and the same goes for females."),
             plotOutput("regressionplot"), br(),
             p("These data points are by state; in order to weed out more confounding 
               variables, I examined the relationship between these two variables 
               on a county level next. These findings are in the next tab.")
             ))
             ),
    
    tabPanel("In-Depth Analysis of Gender Inequality",
             titlePanel("In-Depth Analysis of Gender Inequality"),
             sidebarPanel(
                 p(tags$em("What is the relationship between the unemployment rate and the school
                           dropout rate by sex on a county level? Select a state and one of the 
                           two variables to view the differences in that variable across that 
                           state's counties. The regression plot below also graphs the relationship
                           between the two variables for each state, using county-level data.")),
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
    
    tabPanel("About this Project", 
             fluidRow(column(3), column(6, 
                h3("About this Project", align = "center"),
             p("The goal of this project is to discover the extent to which a 
             school’s inequality mirrors that of its community’s. Although some 
             believe that schools act as the springboard of social equality and 
             progress, others argue that schools merely reinforce the existing 
             stratification found in their societies. In order to test the idea 
             of whether schools serve as social mirrors, I will analyze two main
             forms of inequality, race and gender, in two separate spheres -- 
             the school and the community. Ultimately, I hope to discover 
             whether a correlative relationship exists between inequality in a 
             community and inequality in schools.", align = "center"),
             p("I gathered my data from three sources: the American Community 
             Surveys (ACS) from the US Census Bureau and the National Center 
             for Education Statistics (NCES). From ACS’ data, I extracted information about each 
             state's school dropout rates by gender and  average household 
             income by race. The dropout rates correspond to a school's 
             gender inequality, whereas the household income by race measures 
             a community's racial inequality. From the data from the National 
             Center for Education Statistics (NCES), I retrieved the 
             adjusted cohort graduation rate (ACGR) based on race, which 
             measures a school's racial inequality.", align = "center"),
             column(3))),
             fluidRow(column(3), column(6,    
                h3("About Me", align = "center"),
                p("My name is Katherine, and I plan to concentrate in social studies
                with a secondary in economics.", align = "center"),
                p("Email: katherinewang1@college.harvard.edu", align = "center"),
                p(tags$a(href = "https://github.com/katherinewang122", "GitHub: katherinewang122"), 
                  align = "center"), column(3)))
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$combinedrace1 <- renderImage({
        list(src = "combined_race.gif",
             alt = "combined_race.gif")}, 
            deleteFile = FALSE)
    
    output$combinedsex1 <- renderImage({
        list(src = "combined_sex.gif",
             alt = "combined_sex.gif")}, 
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
            scale_x_continuous(breaks = seq(2, 10, 1), limits = c(2, 10), 
                               labels = c("2%", "3%", "4%", "5%", "6%", "7%", "8%", "9%", "10%")) +
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
