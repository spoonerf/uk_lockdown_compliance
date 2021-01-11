#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(janitor)
library(readr)
library(tidyr)
library(ggplot2)


if(!file.exists(paste0(Sys.Date(),"_Google_Global_Mobility_Report.csv"))){
    download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", 
                  destfile = paste0(Sys.Date(),"_Google_Global_Mobility_Report.csv"))
}

gm <- read.csv(paste0(Sys.Date(),"_Google_Global_Mobility_Report.csv")) %>% 
    filter(country_region == "United Kingdom") %>% 
    dplyr::select( -c(country_region_code))

gm$date <- as.Date(gm$date)

num_days <- Sys.Date() - as.Date("2020-02-15")

#locations <- unique(gm$sub_region_1)
#locations <- locations[nchar(locations) > 0]

locations <- read.csv("gm_locations.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    
    # Application title
    titlePanel("Lockdown Compliance - UK"),
    
    # Sidebar with a select input for locations
    sidebarLayout(position = "right",
                  sidebarPanel(
                      selectInput("region",
                                  "Select a region:",
                                  locations$sub_region_1,
                                  selected = locations$sub_region_1[31]),
                      dateRangeInput("daterange", "Date range:",
                                     start = "2020-02-15",
                                     end   = Sys.Date()),
                      p("The data shown here reflect how communities spent their time during the COVID-19 pandemic. The values are relative to a baseline which is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020.", align = "justified"), 
                      p("Periods of national lockdown are shown with grey bars. Where data are missing there were not sufficient individuals at this location for the data to be aggregated anonymously.", align = "justified"),
                      HTML("<p>All data are taken from <a href='https://www.google.com/covid19/mobility/'> Google Community Mobility Reports</a>.</p>"),
                      HTML("<p><a href ='https://github.com/spoonerf/uk_lockdown_compliance'> Code available on Github</a>.</p>")),
                  # Show a plot 
                  mainPanel(
                      plotOutput("regionPlot")
                  )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$regionPlot <- renderPlot({
        
        gm_region <- gm %>% 
            filter(sub_region_1 == input$region) %>% 
            pivot_longer(., contains("percent")) %>% 
            mutate(name = case_when(name == "retail_and_recreation_percent_change_from_baseline" ~ "Retail & Recreation",
                                    name == "grocery_and_pharmacy_percent_change_from_baseline" ~ "Grocery & Pharmacy",
                                    name == "residential_percent_change_from_baseline" ~ "Residential",
                                    name == "parks_percent_change_from_baseline" ~ "Parks",
                                    name == "transit_stations_percent_change_from_baseline" ~ "Transit Stations",
                                    name == "workplaces_percent_change_from_baseline" ~ "Workplaces"))
        
        if(nrow(gm_region) > as.numeric(num_days)*6){
            
            gm_region <- gm_region %>% 
                group_by(date, sub_region_1, name) %>% 
                summarise(value = mean(value, na.rm = TRUE))
            
        }
        
        
        country <- locations$country[locations$sub_region_1 == input$region]
        
        if(country == "England"){
            lockdowns <- data.frame(x1 = c(as.Date("2020-03-23"), as.Date("2020-11-05"), as.Date("2021-01-05")),
                                    x2 = c(as.Date("2020-07-04"), as.Date("2020-12-02"), Sys.Date()),
                                    y1 = -Inf,
                                    y2 = Inf)
        }
        
        if(country == "Northern Ireland"){
            lockdowns <- data.frame(x1 = c(as.Date("2020-03-28"), as.Date("2020-10-16"), as.Date("2020-11-27"),as.Date("2020-12-26")),
                                    x2 = c(as.Date("2020-07-31"), as.Date("2020-11-20"), as.Date("2020-12-11"),Sys.Date()),
                                    y1 = -Inf,
                                    y2 = Inf)
        }
        
        if(country == "Scotland"){
            lockdowns <- data.frame(x1 = c(as.Date("2020-03-23"), as.Date("2021-01-05")),
                                    x2 = c(as.Date("2020-07-04"), Sys.Date()),
                                    y1 = -Inf,
                                    y2 = Inf)
        }
        
        if(country == "Wales"){
            lockdowns <- data.frame(x1 = c(as.Date("2020-03-23"), as.Date("2020-10-23"), as.Date("2020-12-16")),
                                    x2 = c(as.Date("2020-06-01"), as.Date("2020-11-09"), Sys.Date()),
                                    y1 = -Inf,
                                    y2 = Inf)
        }
        
        # Plotting change from baseline.
        # The baseline is the median value, for the corresponding day of the week, 
        # during the 5-week period Jan 3–Feb 6, 2020
        ggplot()+
            geom_line(data = gm_region %>% filter(date >= input$daterange[1] & date <= input$daterange[2]), aes(x  = date, y = value, group = name, colour = name)) +
            labs(colour = "", y = "Percent change from baseline", x = "Date")+
            geom_hline(yintercept = 0, linetype = "dashed")+
            theme_bw()+
            theme(text = element_text(size=20))+
            theme(legend.position = "none")+
            geom_rect(data = lockdowns, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), alpha=0.4)+
            facet_wrap(.~name,  scales="free_y")+
            ggtitle(input$region)+
            xlim(input$daterange[1], input$daterange[2])
        
        
        
    },
    width = 800, 
    height = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)
