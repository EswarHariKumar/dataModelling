#required libraries to run the project
require(shiny)
require(MASS)
require(plyr)
require(dplyr)
require(GGally)
library(tidyr)
library(leaflet)
library(ggplot2)
require(ggmap)
require(ggalt)
require(ggthemes)
require(rgdal)
require(gridExtra)
require(stringr)

#read the input file
school.country_lookup <- read.csv("school_and_country_table.csv")
#generate give the definaton for asia countries
asiaCounties <- c('China','Japan','Taiwan','Turkey','Israel','Hong Kong',"India","Singapore","South Korea","Macau","Thailand","Iran","Saudi Arabia")
df <- data.frame(asiaCounties)
#order the country
df <- arrange(df,asiaCounties)
#compute the rest of countries by excluding the asian countries
theRest <- subset(school.country_lookup,
                  !school.country_lookup$country %in% asiaCounties)
#order the country
theRestCountry <- arrange(distinct(select(theRest,country)),(country))

#arrange the page. Use the tab layout to representing my visulizations
shinyUI(fluidPage(
  tabsetPanel(
    #The first tab will be a leaflet map showing No 1 university for each country around the world
    tabPanel("World Unversity Map - (2015-2016)",
             div(class="outer", 
                 tags$head(
               # Include  custom CSS
               includeCSS("styles.css")
             ),
             leafletOutput(outputId = "mapPlot",width = "100%", height = "100%"),
             absolutePanel(id = "controls", class = "panel panel-default", 
                           fixed = TRUE,
                           draggable = TRUE, top = 60, left = "auto", 
                           right = 20, bottom = "auto",
                           width = 600, height = "auto",
                           plotOutput(outputId = "barChartUniScore",
                                      height = 500)
                           )
             )),
    tabPanel("Universities in Asian Country vs. The Rest of World Universities (2015-2016)",
             #1st row with 3 columns
             fluidRow(
               #input selection for asian country
               column(3,
                      selectInput('asiaUni','Universities in Asia',
                                  as.character(df$asiaCounties))
               ),
               column(4,offset = 1,
                      #input selection for other country
                      selectInput('theRestUni', 'Universities in the rest of the World',
                                  as.character(theRestCountry$country), 
                                  selected = "Australia")),
               column(3,
                      #input selection for performance Indicators
                      selectInput('pIndicators',"Performance Indicators",
                                  c("Teaching","International","Research",
                                    "Citations","Income")))
             ),
             #2nd row with 2 colums
             fluidRow(
               #scatter plot. 
               #define the double click event and brush event
               column(width=7, 
                      plotOutput(outputId = 'plot1', dblclick = "plot1_dbclick",
                             brush=brushOpts(id="plot1_brush", resetOnNew = TRUE))),
               #bar chat
               column(width=5, plotOutput(outputId = 'plot2'))
             ),
             #3rd row with 2 colums
             fluidRow(
               #parallel co-ordinate plot
               column(width=7, 
                      plotOutput(outputId = 'ggparcoord')),
               #time series plot
               column(width=5,
                      plotOutput(outputId="tsplot"))
             )
             )
  )
  )
  )
  