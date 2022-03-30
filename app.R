#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


if(!require(shiny)) install.packages('shiny')
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(plotly)) install.packages('plotly')
if(!require(albersusa)) install.packages('albersusa')

library(shiny)
library(tidyverse)
library(plotly)
library(albersusa)

cov_demo_counties <- read_csv("covid_demo_counties.csv", 
                                col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                                 cut = col_factor(levels = c("Less than 10%", 
                                                                             "10%-24.9%", "25-50%"))))
hesitancy_county <- read_csv("hesitancy_county.csv")
hesitancy_megacounty <- read_csv("hesitancy_megacounty.csv")
us_county <- counties_sf("laea")
us_states <- usa_sf("laea")
county_hesitancy <- inner_join(us_county, hesitancy_county, by=c("fips"="geo_value")) %>%
    mutate(name = str_replace(name, '\361', 'n'))
megacounty_hesitancy <- left_join(us_states, hesitancy_megacounty, by=c("fips_state"="fips_state"))

race_groups <- list(
    "Nonwhite"="NW",
     "Black or African American"="BAC",
     "American Indian and Alaska Native"="IAC",
     "Asian"="AAC"
 )

race_map <- c(
    "NW" = "Nonwhite",
    "BAC" = "Black or African American",
    "IAC" = "American Indian and Alaska Native",
    "AAC" = "Asian"
)


fill_breaks = c(2, 5, 10, 22, 50, 110, 250, 560, 1250, 2740, 6000)

covidbar <- function(race, date_param) {
    cov_counties_date <- cov_demo_counties %>%
        filter(date == date_param, RACE==race) %>%
        group_by(cut) %>%
        summarize(medianPropInfect = median(propInfect), n=n(), pop=sum(count)) %>%
        mutate(text=paste("<b>",cut, " ", race_map[race], "</b>\nMedian Infection Rate: ", signif(medianPropInfect*100,5),"%\nNum Counties: ", n, "\nNum People: ", pop, sep=""))
    m <- ggplot(cov_counties_date) +
        geom_col(mapping = aes(x=cut, y=medianPropInfect, fill=n+runif(nrow(cov_counties_date), min=0, max=0.0001), text=text)) +
        scale_y_continuous(labels=scales::percent) +
        scale_fill_gradient(low="cornsilk1", high="green4", trans="log", breaks=fill_breaks) +
        labs(x=paste("Percentage of county population that is",race_map[race]), y="Median percentage of infected individuals", fill="Number of\n Counties")
    ggplotly(m, tooltip="text")
}

my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}

covidmap <- function(date_param) {
    county_hesitancy_date <- filter(county_hesitancy, week == date_param) %>%
        mutate(text = paste("<b>", name, "</b>\nHesitancy: ", signif(hesitancy,3) * 100, '%', sep=""))
    megacounty_hesitancy_date <- filter(megacounty_hesitancy, week == date_param) %>%
        mutate(text = paste("<b>", name, "</b>\nHesitancy: ", signif(hesitancy,3) * 100, '%', sep=""))
    m <- ggplot() +
        geom_sf(data=megacounty_hesitancy_date, aes(text=text, fill=hesitancy)) +
        geom_sf(data=county_hesitancy_date, aes(text=text, fill=hesitancy),size=0.3, color="black") +
        my_map_theme() +
        scale_fill_gradient2("Percent\nHesitancy", labels=scales::percent, low="blue", mid="#ffffbf", high="red", limits=c(0, 0.35), midpoint=0.18)
    ggplotly(m, tooltip = c("text")) %>%
        style(hoveron = "fills") 
}

covidplot <- function(tab, race_bar, date_param_bar, date_param_map) {
    if(tab == "Race") {
        covidbar(race_bar, date_param_bar)
    } else {
        covidmap(date_param_map)
    }
}

# Define UI for application that draws a column chart
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 County Data Over Time: Infection Rates and Vaccine Hesitancy"),
    
    # Sidebar with a slider input for date
    tabsetPanel(id = "tab",
                tabPanel(title = "County Affect by Race Demographics", value="Race",
                         selectInput("race_bar",
                                     "Select a racial category:",
                                     choices = race_groups,
                                     selected = "NW"),
                         sliderInput("date_param_bar",
                                     "Select a date to display:",
                                     min = min(cov_demo_counties$date),
                                     max = max(cov_demo_counties$date),
                                     value = min(cov_demo_counties$date))),
                tabPanel(title = "Vaccine Hesitancy by County", value="Hesitancy",
                         sliderInput("date_param_map",
                                     "Select a date to display:",
                                     min = as.Date(min(county_hesitancy$week), "%Y-%m-%d"),
                                     max = as.Date(max(county_hesitancy$week), "%Y-%m-%d"),
                                     value = as.Date(min(county_hesitancy$week), "%Y-%m-%d"),
                                     step = 7))
    ),
    
        
        # Show the map
    mainPanel(
        h3(textOutput("TitleText")),
        h5(textOutput("SubtitleText")),
        plotlyOutput("plot"),
        h5(textOutput("HesitancyInfo")),
        h5("Data sources:"),
        h5(tags$a(href="https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv", 
                  "New York Times US County COVID-19 Data")),
        h5(tags$a(href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html",
                  "US Census Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2019")),
        h5(tags$a(href="https://cmu-delphi.github.io/delphi-epidata/symptom-survey/#covid-19-trends-and-impact-survey", "Delphi's COVIDcast API, COVID-19 Trends and Impact Survey")),
        h5("Graphs inspired by plots in the USA Today Article", 
           tags$a(href="https://www.usatoday.com/in-depth/news/nation/2020/05/02/coronavirus-impact-black-minority-white-neighborhoods-chicago-detroit/3042630001/", 
                  "\"Coronavirus spares one neighborhood but ravages the next. Race and class spell the difference.\""), "and the IHME Covid Collaborative plot ", tags$a(href="https://vaccine-hesitancy.healthdata.org/", "\"Vaccine Hesitancy by County\""))
    )
)

# Define server logic required to draw a barchart
server <- function(input, output) {
    output$TitleText <- renderText({
        ifelse(input$tab == "Race", paste("County Infection Rates by", race_map[input$race_bar], "Population"), "Vaccine Hesitancy by County")
    })
    
    output$SubtitleText <- renderText({
        ifelse(input$tab == "Race", paste("As of", input$date_param_bar), paste("Week of", input$date_param_map))
    })
    
    output$HesitancyInfo <- renderText({
        ifelse(input$tab == "Race", "", "Percentage of population who is unvaccinated, has no appointment to receive a vaccine, and would definitely or probably choose not to get vaccinated if a vaccine were offered.")
    })
    
    output$plot <- renderPlotly({
        covidplot(input$tab, input$race_bar, input$date_param_bar, input$date_param_map)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
