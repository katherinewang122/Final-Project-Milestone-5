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

# I downloaded data for the states, which is already stored in R. 

states <- map_data("state")


# Define UI for application that shows graphs
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    navbarPage(tags$b("School Inequality in America"),
    
    tabPanel("Findings",
             titlePanel("Findings"),
             mainPanel(
                 plotlyOutput("unemprace"),
                 br(), br(), br(), br(),
                 plotlyOutput("gradrace"),
                 br(), br(), br(), br(),
                 plotlyOutput("unempsex"),
                 br(), br(), br(), br(),
                 plotlyOutput("gradsex")
             )
             ),
    
    tabPanel("In-Depth Analysis",
             titlePanel("In-Depth Analysis"),
             mainPanel(
                 plotlyOutput("regressionplot"),
                 br(), br(),
                 plotlyOutput("regressionplot1")
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
server <- function(input, output, session) {
    output$gradrace <- renderPlotly({
        # I removed the first row since it shows the graduation rate for the entire
        # nation, not the states. I then created a new column, region, which transformed
        # all the states' names into lowercase letters, which will facilitate combining
        # it with the states data. Next, I made four individual tibbles for the four
        # different races. For each of these tibbles, I created a new variable, race,
        # which will help me facet them later on. I also renamed all of their
        # percentages into a common variable, pct, so that I can combine them.
        
        race_grad <- nces2017[-1,]
        race_grad$region <- tolower(race_grad$"1")
        combined_race <- left_join(states, race_grad, by = "region")
        
        white <- race_grad %>% 
            mutate(race = "White", pct = race_grad$"9") %>% 
            select(region, race, pct)
        
        black <- race_grad %>% 
            mutate(race = "Black", pct = race_grad$"10") %>% 
            select(region, race, pct)
        
        hispanic <- race_grad %>% 
            mutate(race = "Hispanic", pct = race_grad$"11") %>% 
            select(region, race, pct)
        
        asian <- race_grad %>% 
            mutate(race = "Asian", pct = race_grad$"12") %>% 
            select(region, race, pct)
        
        # Since I only know how to use rbind on two tibbles at a time, I used a separate
        # rbind function each time I joined two new tibbles to each other. For the final
        # combination, I used a left join to link the states data with the combined race
        # data, using their common variable, region.
        
        combineda <- rbind(white, black)
        combinedb <- rbind(combineda, hispanic)
        combinedc <- rbind(combinedb, asian)
        combined_final <- left_join(states, combinedc, by = "region")
            
        p1 <- ggplot(combined_final, aes(x = long, y = lat, group = group, 
                                         fill = pct, frame = race)) +
            geom_polygon(color = "white") +
            scale_fill_viridis(option = "magma", 
                               direction = -1, 
                               breaks = c(70, 80, 90), 
                               labels = c("70%", "80%", "90%")) +
            theme_void() +
            coord_fixed(1.3) +
            labs(title = "School Graduation Rate by Race in 2017",
                 caption = "Source: National Center for Education Statistics 2017",
                 fill = "Graduation Rate")
        
        p1 <- p1 %>%
            animation_opts(1000, easing = "linear", redraw = FALSE) %>%
            animation_slider(
                currentvalue = list(prefix = "Race: ", font = list(color = "black"))
            )
        ggplotly(p1)
    })
    
    output$gradsex <- renderPlotly({
        # My first task was to create a map displaying school graduation rates for the
        # male population. I filtered the ACS data, selecting the three variables I
        # needed, and created a new variable, grad_rate, to help me measure the percent
        # of those who graduated. I created another variable, type, to remind me that
        # this was for the male population, just in case I choose to combine the female
        # and male datasets later on. I renamed some variables into names that held
        # meaning for the user.
        
        male <- acs %>% 
            select(geo_name, 
                   se_a12003a_001, 
                   se_a12003a_002) %>% 
            mutate(grad_rate = 100 - (se_a12003a_002/se_a12003a_001)*100) %>% 
            mutate(type = "male") %>% 
            mutate(total = se_a12003a_001, drop = se_a12003a_002) %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        male$region <- tolower(male$geo_name)
        
        # I creatd a new male graduation rate tibble that only selected the variables I
        # needed.
        
        male_new <- male %>% 
            select(region, total, drop, grad_rate, type)
        
        female <- acs %>% 
            select(geo_name, 
                   se_a12003b_001, 
                   se_a12003b_002) %>% 
            mutate(grad_rate = 100 - (se_a12003b_002/se_a12003b_001)*100) %>% 
            mutate(type = "female") %>% 
            mutate(total = se_a12003b_001, drop = se_a12003b_002) %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        female$region <- tolower(female$geo_name)
        
        # I creatd a new female graduation rate tibble that only selected the variables
        # I needed.
        
        female_new <- female %>% 
            select(region, total, drop, grad_rate, type)
        
        # I first combined the female and male graduation rates tables into a new tibble
        # called "both." Then I joined "both" with the states data to get "combo."
        both <- rbind(female_new, male_new)
        combo <- left_join(states, both, by = "region") %>% 
            mutate_if(is.numeric, ~round(., 1))
        
        p2 <- ggplot(combo, aes(x = long, y = lat, group = group, fill = grad_rate)) +
            geom_polygon(color = "white") +
            coord_fixed(1.3) +
            labs(title = "School Graduation Rates by Gender in 2017",
                 subtitle = "For Population 16-19 Years",
                 caption = "Source: American Community Survey 2017",
                 fill = "Graduation Rate") +
            scale_fill_viridis(option = "viridis", 
                               direction = -1, 
                               guide = guide_colorbar(direction = "horizontal",
                                                      barheight = unit(2, units = "mm"),
                                                      barwidth = unit(35, units = "mm"),
                                                      draw.ulim = FALSE,
                                                      title.position = "top",
                                                      title.hjust = 0.5,
                                                      label.hjust = 0.5),
                               breaks = c(93, 95, 97), 
                               labels = c("93%", "95%", "97%")) +
            facet_wrap(~ type) +
            theme_void() +
            theme(legend.position = "bottom", 
                  plot.title = element_text(size = 10),
                  plot.subtitle = element_text(size = 8))
        
        ggplotly(p2)
    })
    
    output$regressionplot <- renderPlotly({
        pov_status_race <- pov_status %>% 
            mutate(white = (se_a13001a_002/se_a13001a_001) * 100) %>% 
            mutate(black = (se_a13001b_002/se_a13001b_001) * 100) %>% 
            mutate(asian = (se_a13001d_002/se_a13001d_001) * 100) %>% 
            mutate(hispanic = (se_a13001h_002/se_a13001h_001) * 100)
        
        combined_final1 <- combined_final %>% 
            mutate(geo_name = region) %>% 
            mutate(drop_rate = 100 - pct)
        
        pov_status_joined <- left_join(pov_status_race, combined_final1, by = "geo_name") %>% 
            select(geo_name, white, black, asian, hispanic, drop_rate, race)
        
        statenames <- unique(pov_status_joined$geo_name)
        
        pov_status_final <- vector()
        for(i in 1:length(statenames)){
            sub <- unique(pov_status_joined[pov_status_joined$geo_name == statenames[i], ])
            addition <- c(snames[i], unlist(sub[1, 2:5]), sub$drop_rate[c(1, 2, 4, 3)])
            pov_status_final <- rbind(pov_status_final, addition)
        }
        pov_status_final <- as.data.frame(pov_status_final)
        colnames(pov_status_final) <- c('state', 'whitepov', 'blackpov', 'asianpov', 'hisppov',
                                        'whitedo', 'blackdo', 'asiando', 'hispdo')
        pov_status_final[, -1] <- apply(pov_status_final[, -1], 2, as.numeric)
        pov_status_final <- 
            pov_status_final %>% na.omit() %>% 
            mutate_if(is.numeric, ~round(., 1))
        pov_status_final1 <- pov_status_final %>% 
            mutate_if(is.numeric, ~round(., 1))
        pov_status_final2 <- pov_status_final %>% 
            mutate_if(is.numeric, ~round(., 1))
        pov_status_final3 <- pov_status_final %>% 
            mutate_if(is.numeric, ~round(., 1))
        
        p3 <- ggplot() +
            geom_point(data = pov_status_final, aes(x = whitepov, y = whitedo), col = "darkolivegreen", alpha = 0.8) +
            geom_smooth(data = pov_status_final, aes(x = whitepov, y = whitedo), method = "lm", se = F, col = "darkolivegreen", alpha = 0.8) +
            geom_point(data = pov_status_final1, aes(x = blackpov, y = blackdo), col = "deeppink2", alpha = 0.8) +
            geom_smooth(data = pov_status_final1, aes(x = blackpov, y = blackdo), method = "lm", se = F, col = "deeppink2", alpha = 0.8) +
            geom_point(data = pov_status_final2, aes(x = asianpov, y = asiando), col = "goldenrod2", alpha = 0.8) +
            geom_smooth(data = pov_status_final2, aes(x = asianpov, y = asiando), method = "lm", se = F, col = "goldenrod2", alpha = 0.8) +
            geom_point(data = pov_status_final3, aes(x = hisppov, y = hispdo), col = "midnightblue", alpha = 0.8) +
            geom_smooth(data = pov_status_final3, aes(x = hisppov, y = hispdo), method = "lm", se = F, col = "midnightblue", alpha = 0.8) +
            scale_x_continuous(breaks = seq(5, 45, 5), limits = c(5, 45)) +
            scale_y_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
            labs(title = "Relationship Between Poverty Status and School Dropout Rate \n Based on Race in 2017",
                 x = "Percentage Whose Income is Below Poverty Level",
                 y = "School Dropout Rate",
                 caption = "Source: American Community Survey 2017 and National Center for Education Statistics 2017") +
            theme(legend.position = "right") +
            annotate("text", x = 40, y = 15, label = "Race", color = "black", size = 5) +
            annotate("text", x = 41, y = 12, label = "White", color = "black", size = 4) +
            annotate("text", x = 41, y = 10, label = "Black", color = "black", size = 4) +
            annotate("text", x = 41, y = 8, label = "Asian", color = "black", size = 4) +
            annotate("text", x = 41.5, y = 6, label = "Hispanic", color = "black", size = 4) +
            geom_segment(aes(x = 37.5, y = 12, xend = 39, yend = 12), col = "darkolivegreen", data = pov_status_final) +
            geom_segment(aes(x = 37.5, y = 10, xend = 39, yend = 10), col = "deeppink2", data = pov_status_final) +
            geom_segment(aes(x = 37.5, y = 8, xend = 39, yend = 8), col = "goldenrod", data = pov_status_final) +
            geom_segment(aes(x = 37.5, y = 6, xend = 39, yend = 6), col = "midnightblue", data = pov_status_final) +
            theme_classic() +
            theme(plot.title = element_text(family = "serif", size = 20),
                  axis.text.y = element_text(size = 16), 
                  axis.text.x = element_text(size = 16), 
                  axis.title.y = element_text(size = 16),
                  axis.title.x = element_text(size = 16))
        ggplotly(p3)
    })
    
    output$regressionplot1 <- renderPlotly({
        # starts the pov status regression part
        pov_status_sex <- pov_status %>% 
            mutate(male = (se_a13005_003/se_a13005_001) * 100) %>% 
            mutate(female = (se_a13005_004/se_a13005_001) * 100)
        
        male <- acs %>% 
            select(geo_name, 
                   se_a12003a_001, 
                   se_a12003a_002) %>% 
            mutate(grad_rate = 100 - (se_a12003a_002/se_a12003a_001)*100) %>% 
            mutate(type = "male") %>% 
            mutate(total = se_a12003a_001, drop = se_a12003a_002) %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        male$region <- tolower(male$geo_name)
        
        # for females
        female <- acs %>% 
            select(geo_name, 
                   se_a12003b_001, 
                   se_a12003b_002) %>% 
            mutate(grad_rate = 100 - (se_a12003b_002/se_a12003b_001)*100) %>% 
            mutate(type = "female") %>% 
            mutate(total = se_a12003b_001, drop = se_a12003b_002) %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        female$region <- tolower(female$geo_name)
        
        female1 <- female
        female1$geo_name = tolower(female1$geo_name)
        
        pov_status_female <- left_join(pov_status_sex, female1, by = "geo_name") %>% 
            mutate(drop_rate = (se_a12003b_002/se_a12003b_001)*100) %>% 
            select(geo_name, female, drop_rate) %>% 
            mutate_if(is.numeric, ~round(., 1))
        
        male1 <- male
        male1$geo_name = tolower(male1$geo_name)
        
        pov_status_male <- left_join(pov_status_sex, male1, by = "geo_name") %>% 
            mutate(drop_rate = (se_a12003a_002/se_a12003a_001)*100) %>% 
            select(geo_name, male, drop_rate) %>% 
            mutate_if(is.numeric, ~round(., 1))
        
        # excluded two outliers because they unduly influenced the regression line
        pov_status_male <- pov_status_male[pov_status_male$male < max(pov_status_male$male),]
        pov_status_female <- pov_status_female[pov_status_female$female < max(pov_status_female$female),]
        
        p4 <- ggplot() +
            geom_point(data = pov_status_male, aes(x = male, y = drop_rate), col = "blue", alpha = 0.8) +
            geom_smooth(data = pov_status_male, aes(x = male, y = drop_rate), method = "lm", se = F, col = "blue", alpha = 0.8) +
            geom_point(data = pov_status_female, aes(x = female, y = drop_rate), col = "deeppink2", alpha = 0.8) +
            geom_smooth(data = pov_status_female, aes(x = female, y = drop_rate), method = "lm", se = F, col = "deeppink2", alpha = 0.8) +
            labs(title = "Relationship Between Poverty Status and School Dropout Rate \n Based on Sex in 2017",
                 x = "Percentage Whose Income is Below Poverty Level",
                 y = "School Dropout Rate",
                 caption = "Source: American Community Survey 2017 and National Center for Education Statistics 2017") +
            annotate("text", x = 20, y = 3.5, label = "Sex", color = "black", size = 6) +
            annotate("text", x = 21, y = 3, label = "Male", color = "black", size = 5.5) +
            annotate("text", x = 21.5, y = 2.8, label = "Female", color = "black", size = 5.5) +
            geom_segment(aes(x = 19, y = 3, xend = 20, yend = 3), col = "blue", data = pov_status_male) +
            geom_segment(aes(x = 19, y = 2.8, xend = 20, yend = 2.8), col = "deeppink2", data = pov_status_female) +
            theme_classic()
        ggplotly(p4)
    })
    
    output$unempsex <- renderPlotly({
        
        # for males
        male <- unemp_data %>% 
            select(geo_name, 
                   se_a17005a_001, 
                   se_a17005a_003) %>% 
            mutate(unemp = (se_a17005a_003/se_a17005a_001)*100) %>% 
            mutate(type = "male") %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        male$region <- tolower(male$geo_name)
        
        male <- male %>% 
            select(region, unemp, type)
        
        # for females
        female <- unemp_data %>% 
            select(geo_name, 
                   se_a17005b_001, 
                   se_a17005b_003) %>% 
            mutate(unemp = (se_a17005b_003/se_a17005b_001)*100) %>% 
            mutate(type = "female") %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        female$region <- tolower(female$geo_name)
        
        female <- female %>% 
            select(region, unemp, type)
        
        # I first combined the female and male graduation rates tables into a new tibble
        # called "both." Then I joined "both" with the states data to get "combo."
        both <- rbind(female, male)
        combo <- left_join(states, both, by = "region") %>% 
            mutate_if(is.numeric, ~round(., 1))
        
        ggplot(combo, aes(x = long, y = lat, group = group, fill = unemp)) +
            geom_polygon(color = "white") +
            coord_fixed(1.3) +
            labs(title = "Unemployment Rates by Gender in 2017",
                 subtitle = "For Population 16 Years and Over",
                 caption = "Source: American Community Survey 2017",
                 fill = "Unemployment Rate") +
            scale_fill_viridis(option = "viridis", 
                               direction = -1, 
                               guide = guide_colorbar(direction = "horizontal",
                                                      barheight = unit(2, units = "mm"),
                                                      barwidth = unit(35, units = "mm"),
                                                      draw.ulim = FALSE,
                                                      title.position = "top",
                                                      title.hjust = 0.5,
                                                      label.hjust = 0.5),
                               breaks = c(4, 6, 8), 
                               labels = c("4%", "6%", "8%")) +
            facet_wrap(~ type) +
            theme_void() +
            theme(legend.position = "bottom", 
                  plot.title = element_text(size = 10),
                  plot.subtitle = element_text(size = 8))
    })
    
    output$unemprace <- renderPlotly({
        # for males
        white <- unemp_data %>% 
            select(geo_name, 
                   se_a17006a_001, 
                   se_a17006a_003) %>% 
            mutate(unemp = (se_a17006a_003/se_a17006a_001)*100) %>% 
            mutate(type = "white") %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        white$region <- tolower(white$geo_name)
        
        white <- white %>% 
            select(region, unemp, type)
        
        # for females
        black <- unemp_data %>% 
            select(geo_name, 
                   se_a17006b_001, 
                   se_a17006b_003) %>% 
            mutate(unemp = (se_a17006b_003/se_a17006b_001)*100) %>% 
            mutate(type = "black") %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        black$region <- tolower(black$geo_name)
        
        black <- black %>% 
            select(region, unemp, type)
        
        # asian
        asian <- unemp_data %>% 
            select(geo_name, 
                   se_a17006d_001, 
                   se_a17006d_003) %>% 
            mutate(unemp = (se_a17006d_003/se_a17006d_001)*100) %>% 
            mutate(type = "asian") %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        asian$region <- tolower(asian$geo_name)
        
        asian <- asian %>% 
            select(region, unemp, type)
        
        # hispanic
        hispanic <- unemp_data %>% 
            select(geo_name, 
                   se_a17006h_001, 
                   se_a17006h_003) %>% 
            mutate(unemp = (se_a17006h_003/se_a17006h_001)*100) %>% 
            mutate(type = "hispanic") %>% 
            mutate(region = geo_name)
        
        # I then turned all the states in the geo_name column into lowercase letters so
        # that it would facilitate the left_join function that I would later use.
        
        hispanic$region <- tolower(hispanic$geo_name)
        
        hispanic <- hispanic %>% 
            select(region, unemp, type)
        
        # I first combined the female and male graduation rates tables into a new tibble
        # called "both." Then I joined "both" with the states data to get "combo."
        both <- rbind(white, black, asian, hispanic)
        combined <- left_join(states, both, by = "region") %>% 
            mutate_if(is.numeric, ~round(., 1))
        
        ggplot(combined, aes(x = long, y = lat, group = group, fill = unemp)) +
            geom_polygon(color = "white") +
            coord_fixed(1.3) +
            labs(title = "Unemployment Rates by Race in 2017",
                 subtitle = "For Population 16 Years and Over",
                 caption = "Source: American Community Survey 2017",
                 fill = "Unemployment Rate") +
            scale_fill_viridis(option = "plasma", 
                               direction = -1, 
                               guide = guide_colorbar(direction = "horizontal",
                                                      barheight = unit(2, units = "mm"),
                                                      barwidth = unit(35, units = "mm"),
                                                      draw.ulim = FALSE,
                                                      title.position = "top",
                                                      title.hjust = 0.5,
                                                      label.hjust = 0.5),
                               breaks = c(4, 8, 12, 16), 
                               labels = c("4%", "8%", "12%", "16%")) +
            facet_wrap(~ type) +
            theme_void() +
            theme(legend.position = "bottom", 
                  plot.title = element_text(size = 10),
                  plot.subtitle = element_text(size = 8))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
