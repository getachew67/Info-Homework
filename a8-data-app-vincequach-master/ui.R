library("dplyr")
library("shiny")
library("ggplot2")
library("maps")
library("plotly")

my_ui <- fluidPage(
   titlePanel("Eviction and Poverty Data in Washington"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Choose a year",
                     value = 2016, 
                     min = 2000,
                     max = 2016
            
         ),
         br(),
         selectInput("filter",
                     "Choose a filter:",
                     c("Evictions" = "Evictions",
                       "Poverty Rate" = "Poverty Rate"))
      ),
   mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("table"), p(textOutput("table_info"))),
                  tabPanel("Visualization", plotlyOutput("map"), p(textOutput("map_info")))
                  )
      )
   ),
   p(h2("Resources")),
   p("This research uses data from The Eviction Lab at Princeton University, 
      a project directed by Matthew Desmond and designed by Ashley Gromis, Lavar Edmonds, James Hendrickson, 
      Katie Krywokulski, Lillian Leung, and Adam Porton. The Eviction Lab is funded by the JPB, Gates, and 
      Ford Foundations as well as the Chan Zuckerberg Initiative. More information is found at evictionlab.org.")
)


shinyUI(my_ui)