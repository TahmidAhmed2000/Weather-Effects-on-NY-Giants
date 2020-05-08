#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(broom)
library(gt)
library(readr)

joined <- readRDS("joined.rds")
giants_weather <- readRDS("giants_weather.rds")
EM <- readRDS("EM.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Added theme that had NY Giants colors
    theme = shinytheme("superhero"),
    
    # Brief Description of the model of my joined data of stadium attendance and temperature.
    
    navbarPage("Weather Effects on NY Giants",
               tabPanel("Stadium Attendance",
                        tabPanel("Graphics",
                                 br(),
                                 
                                 # Added selectInput panel, so viewers can choose different weather measurements.
                                 
                                 sidebarPanel(
                                     h4("Regressions"),
                                     p("These plots show how attendance is
                                     impacted by different weather conditions."),
                                     
                                     helpText("Use the options to view different regressions"),
                                     selectInput(inputId = "weather",
                                                 label = "Variable:",
                                                 choices = c("Average Temperature" = "avg_temp",
                                                             "Average Dewpoint" = "avg_dewpoint",
                                                             "Average Humidity" = "avg_humidity",
                                                             "Average Wind Speed" = "avg_wind"),
                                                 selected = "Average Temperature")),
                                 mainPanel(
                                     
                                     # Adding gt tables with confidence intervals
                                     
                                     tabsetPanel(id = "tabsMain",
                                                 tabPanel("Plots",
                                                          br(),
                                                          plotOutput("attplot"),
                                                          br(),
                                                          p("The plots show the regressions of different weather variables on stadium attendance. It appears that the there is no relation between weather and stadium attendance. Variables other than temperature are tested because there is a misconception that only temperature can play a role in football performance.
                                                            What is interesting is that since there is no relation between weather and stadium attendance, this shows that people seem to not be affected by weather when paying to attend games")
                                                 ),
                                                 tabPanel("Models",
                                                          br(),
                                                          gt_output("att_model"),
                                                          br(),
                                                          br(),
                                                          p(paste("The regression model shows the regression coefficients and their confidence intervals for respective weather 
                                                             variables on stadium attendance. The coefficient is essentially the Average Treatment Effect
                                                             of increasing the given weather variable by one unit on stadium attendance. Furthermore, a Bayesian interpretation is 
                                                             that we are 95% certain that the true value of the ATE is within this confidence interval.")),
                                ))))),
               
    # Title of new tab focusing on how weather affects the overall team        
             tabPanel(
                 title = "Overall Team",
                 h2(
                    "Points Scored Over Time"
                    ),
             p("Use the options in the side panel to view the graph comparing points scored over time while changing temperature"
               ),
    
    # Added a slider to adjust temperature to see how Giants performed historically under certain weather
    
             sliderInput(
                 inputId = "avg_temp",
                 label = "Temperature",
                 min = 14,
                 max = 86,
                 value = c(30, 50)
             ),
             checkboxInput(
                 inputId = "line",
                 label = "Show Best Fit Line",
                 value = FALSE
             ),
             mainPanel(plotOutput("timePlot")
             )),
             
     # New tab for how weather affects the Quarterback
             
             tabPanel(("Quarterback"),
                      tabPanel("Graphics",
                               br(),
                               
                               # Added selectInput panel, so viewers can choose different weather measurements.
                               
                               sidebarPanel(
                                   h4("Regressions"),
                                   p("These plots show how Eli Manning is
                                     impacted by different weather conditions."),
                                   
                                   helpText("Use the options to view different regressions"),
                               selectInput(inputId = "weathera",
                                               label = "Variable:",
                                               choices = c("Average Temperature" = "avg_temp",
                                                           "Average Dewpoint" = "avg_dewpoint",
                                                           "Average Humidity" = "avg_humidity",
                                                           "Average Wind Speed" = "avg_wind"),
                                               selected = "Average Temperature")),
                               mainPanel(
                                   
                               # Adding gt tables with confidence intervals
                                   
                                   tabsetPanel(id = "tabsMain",
                                               tabPanel("Plots",
                                                        br(),
                                                        plotOutput("EMplot"),
                                                        br(),
                                                        p("The plots show the regressions of different weather variables on Eli Manning's yardage. It appears that the variables have a somewhat moderate relationship with yardage. Variables other than temperature are tested because there is a misconception that only temperature can play a role in football performance.")
                                               ),
                                               tabPanel("Models",
                                                        br(),
                                                        gt_output("em_model"),
                                                        br(),
                                                        br(),
                                                        p(paste("The regression model shows the regression coefficients and their confidence intervals for respective weather 
                                                             variables on Eli Manning's yardage. The coefficient is essentially the Average Treatment Effect
                                                             of increasing the given weather variable by one unit on Eli Manning's yardage in a game.")),
                                                        ))))),
                                                        
                                            
    # Created an About tab to describe project
    
                tabPanel(title = "About", h3('Background'),
                                            br(),
                                            p("My name is Tahmid Ahmed. In this project, I am focusing on the effects of weather on the New York Giants performance and attendance of games. The weather data and game logs of players are sourced from the Github account, Nolanole. The data contains weather characteristics like average temperature, dewpoint, humidity, etc. Furthermore, I have joined the weather data and player statistics with stadium attendance to see how weather affects attendance of games. The attendance data is sourced from the Github profile, Rfordatascience. I am from New York and the Giants are my favorite team, and I thought it would be interesting to focus on my favorite team. 
                          You can find the code to this project on my ",
                                              a("GitHub",
                                                href = "https://github.com/TahmidAhmed2000"),
                                              "account page. My email is tahmidahmed@college.harvard.edu."))))
                

# Define server logic required to draw plots and graphs

server <- function(input, output) {

    # Created output plot for first tab, using ggplot
    
    output$attplot <- renderPlot({
        
        
        if(input$weather == "avg_temp") {
            x_value <- joined$avg_temp
            x_lab <- "Average Temperature"
            EM_title <- "Weather Impact on Stadium Attendance During the Regular Season"
        } 
        else if(input$weather == "avg_dewpoint") {
            x_value <- joined$avg_dewpoint
            x_lab <- "Average Dewpoint"
            EM_title <- "Weather Impact on Stadium Attendance During the Regular Season"
        } 
        else if(input$weather == "avg_humidity") {
            x_value <- joined$avg_humidity
            x_lab <- "Average Humidity"
            EM_title <- "Weather Impact on Stadium Attendance During the Regular Season"
        }
        else if(input$weather == "avg_wind") {
            x_value <- joined$avg_wind
            x_lab <- "Average Wind Speed"
            EM_title <- "Weather Impact on Stadium Attendance During the Regular Season"
        }
        
        # ggplot of ELi Manning using my created variables
        
        ggplot(joined, aes(x_value, weekly_attendance, color = precipitation)) +
            geom_point() +
            geom_smooth(method = "lm", se = F, color = "black") +
            labs(y = "Yards",
                 x = x_lab,
                 title = EM_title,
                 subtitle = "Looking at data since 2010") +
            theme_bw()
        
    })
    
    
    # To create the gt regression tables, I used else if statements to get more regressions. 
    
    output$att_model <- render_gt({
        
        if(input$weather == "avg_temp") {
            joined %>% 
                lm(weekly_attendance ~ avg_temp, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Temperature")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        } 
        else if(input$weather == "avg_dewpoint") {
            joined %>% 
                lm(weekly_attendance ~ avg_dewpoint, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Dewpoint")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        } 
        else if(input$weather == "avg_humidity") {
            joined %>% 
                lm(weekly_attendance ~ avg_humidity, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Humidity")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)  
        }
        
        else if(input$weather == "avg_wind") {
            joined %>% 
                lm(weekly_attendance ~ avg_wind, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Wind Speed")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)  
        }
    }) 
    
    # Created plot for second tab, using ggplot. However, I coded the option to add a regression line.
    
    output$timePlot <- renderPlot({plottime <- giants_weather %>%
        filter(avg_temp >= input$avg_temp[1] & avg_temp <= input$avg_temp[2]) %>%
        ggplot(aes(date, giants_points, color = precipitation)) + 
        geom_point() +
        theme_bw() +
        labs(title = "Temperature's Effect on Points Scored Over Time",
             subtitle = "looking at data since 2010") +
        xlab("Year") +
        ylab("Points Scored")
    
    if (input$line == TRUE) {
        plottime <-
            plottime + geom_smooth(method = "lm",
                                           se = FALSE,
                                           lty = 2)
    }
    plottime
})
    
    # Created output plot for Eli Manning, and creating the different options for weather. 
    
    output$EMplot <- renderPlot({
        
        
        if(input$weathera == "avg_temp") {
            x_value <- EM$avg_temp
            x_lab <- "Average Temperature"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        } 
        else if(input$weathera == "avg_dewpoint") {
            x_value <- EM$avg_dewpoint
            x_lab <- "Average Dewpoint"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        } 
        else if(input$weathera == "avg_humidity") {
            x_value <- EM$avg_humidity
            x_lab <- "Average Humidity"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        }
        else if(input$weathera == "avg_wind") {
            x_value <- EM$avg_wind
            x_lab <- "Average Wind Speed"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        }
        
        # ggplot of ELi Manning using my created variables
        
        ggplot(EM, aes(x_value, yards, size = tds, color = precipitation)) +
            geom_point() +
            geom_smooth(method = "lm", se = F, color = "black") +
            labs(y = "Yards",
                 x = x_lab,
                 title = EM_title) +
            theme_bw()
        
    })


    # To create the gt regression tables, I used else if statements to get more regressions. 
    
    output$em_model <- render_gt({
        
        if(input$weathera == "avg_temp") {
            EM %>% 
                lm(yards ~ avg_temp, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Temperature")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        } 
        else if(input$weathera == "avg_dewpoint") {
            EM %>% 
                lm(yards ~ avg_dewpoint, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Dewpoint")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        } 
        else if(input$weathera == "avg_humidity") {
            EM %>% 
                lm(yards ~ avg_humidity, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Humidity")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)  
        }

        else if(input$weathera == "avg_wind") {
            EM %>% 
                lm(yards ~ avg_wind, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Average Wind Speed")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Yardage") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)  
        }
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
