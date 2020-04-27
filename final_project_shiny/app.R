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
    # Information of model
    navbarPage("Weather Effects on NY Giants",
               tabPanel("Stadium Attendance", mainPanel(h1('Stadium Attendance Regression'),
                                                        p("This plot joined both weather data and stadium attendance of the New York Giants. It can be seen that there over time, there is no strong correlation between average temperature and stadium attendance. While the relationship is not significant, this plot shows that fans will attend games no matter the temperature."),
                                                        p("Another notable aspect of the plot is that games with 90,000 attendees seems to be outliers. They are outliers because this is when the Giants play in the Cowboys stadium, which is one of the stadiums that holds the most amount of fans."),
                                                        p("After conducting a regression of temperature and stadium attendance, I get a correlation of -.03, which shows that there weekly attendance and temperature have a week relation. Furthermore, the regression coefficient for avg_temp was -16.08 with a p-value of .734. Given that the p-value is also greater than 0.05, based on the p-value test, this coefficient is not statistically significant."),
                                                        plotOutput("attendanceplot"))),
               
                
             tabPanel(
                 title = "Overall Team",
                 h2(
                    "Points Scored Over Time"
                    ),
             p("Use the options in the side panel to view the graph comparing points scored over time while changing temperature"
               ),
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
             
             # Eli Manning
             
             tabPanel("Quarterback",
                      tabPanel("Graphics",
                               br(),
                               
                               sidebarPanel(
                                   h4("Regressions"),
                                   p("These plots show how Eli Manning is
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
                                                        
                                            
                         
                tabPanel(title = "About", h3('Background'),
                                            br(),
                                            p("My name is Tahmid Ahmed. In this project, I am focusing on the effects of weather on the New York Giants performance and attendance of games. The weather data and game logs of players are sourced from the Github account, Nolanole. The data contains weather characteristics like average temperature, dewpoint, humidity, etc. Furthermore, I have joined the weather data and player statistics with stadium attendance to see how weather affects attendance of games. The attendance data is sourced from the Github profile, Rfordatascience. I am from New York and the Giants are my favorite team, and I thought it would be interesting to focus on my favorite team. 
                          You can find the code to this project on my ",
                                              a("GitHub",
                                                href = "https://github.com/TahmidAhmed2000"),
                                              "account page. My email is tahmidahmed@college.harvard.edu."))))
                
               
                           
                         
                                            
                         


             

    
                      
                               
             
             
                       

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$attendanceplot <- renderPlot({plot1 <- joined %>%
        ggplot(aes(avg_temp, weekly_attendance)) +
        geom_point() + geom_smooth(method = "lm") +
        theme_classic() +
        labs(title = "Weather's Impact on Attendance of Giants Games",
             subtitle = "Appears to be no correlation") +
        ylab("Attendance") +
        xlab("Temperature")
    plot1
    })
    
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
    
    output$EMplot <- renderPlot({
        
        
        if(input$weather == "avg_temp") {
            x_value <- EM$avg_temp
            x_lab <- "Average Temperature"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        } 
        else if(input$weather == "avg_dewpoint") {
            x_value <- EM$avg_dewpoint
            x_lab <- "Average Dewpoint"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        } 
        else if(input$weather == "avg_humidity") {
            x_value <- EM$avg_humidity
            x_lab <- "Average Humidity"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        }
        else if(input$weather == "avg_wind") {
            x_value <- EM$avg_wind
            x_lab <- "Average Wind Speed"
            EM_title <- "Weather Impact on Eli Manning During the Regular Season"
        }
        
        # ggplot using my created variables
        
        ggplot(EM, aes(x_value, yards, size = tds, color = precipitation)) +
            geom_point() +
            geom_smooth(method = "lm", se = F, color = "black") +
            labs(y = "Yards",
                 x = x_lab,
                 title = EM_title) +
            theme_bw()
        
    })


    # for the gt regression tables I wanted more flexibilty for changing 
    # titles etc so each plot is in its own if clasue to make changes 
    # between them
    
    output$em_model <- render_gt({
        
        if(input$weather == "avg_temp") {
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
        else if(input$weather == "avg_dewpoint") {
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
        else if(input$weather == "avg_humidity") {
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

        else if(input$weather == "avg_wind") {
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
