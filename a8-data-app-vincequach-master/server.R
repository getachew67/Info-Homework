library("dplyr")
library("shiny")
library("ggplot2")
library("maps")
library("plotly")

# Server function
my_server <- function(input, output) {
   # Reactive function for table
   reactive_table <- reactive({
      reactive_year <- input$year
      reactive_topic <- input$filter
      
      eviction_counties <- read.csv(file = "data/evictionlab-us-counties.csv", stringsAsFactors = FALSE)
      eviction_poverty <- filter(eviction_counties, parent.location == "Washington")
      eviction_poverty <- filter(eviction_poverty, year == reactive_year)
      eviction_poverty <- select(eviction_poverty, year, name, population, poverty.rate, median.household.income, rent.burden, eviction.filings, evictions)
      names <- c("Year", "County", "Population", "Poverty Rate", "Median Household Income", "Rent Burden (% of income towards rent)", "Eviction Filings", "Evictions")
      colnames(eviction_poverty) <- names
     
      # Choosing the output table based on what is chosen
      output_table <- select(eviction_poverty, Year, County, Population, `Median Household Income`, reactive_topic)
      return(output_table)
   })
   output$table <- renderTable(reactive_table())
   
   # Reactive functionfor map
   reactive_map <- reactive({
      reactive_year <- input$year
      reactive_topic <- input$filter
      eviction_counties <- read.csv(file = "data/evictionlab-us-counties.csv", stringsAsFactors = FALSE)
      eviction_poverty <- filter(eviction_counties, parent.location == "Washington")
      eviction_poverty <- filter(eviction_poverty, year == reactive_year)
      eviction_poverty <- select(eviction_poverty, year, name, population, poverty.rate, 
                                 median.household.income, rent.burden, eviction.filings, evictions)
      names <- c("Year", "County", "Population", "Poverty Rate", "Median Household Income", 
                 "Rent Burden (% of income towards rent)", "Eviction Filings", "Evictions")
      colnames(eviction_poverty) <- names
      
      WA <- map_data("county") %>%
         filter(region == "washington")
      
      # Functions to make the first letter of each county toupper
      upper_first <- function(s) {
         paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
      }
      WA$subregion <- upper_first(WA$subregion)
      WA$region <- upper_first(WA$region)
      add_county <- function(s) {
         paste(s, "County", sep = " ")
      }
      WA$subregion <- add_county(WA$subregion)
      
      # Renaming columns
      WA <- rename(WA, "State" = region)
      WA <- rename(WA, "County" = subregion)
      
      # Joining to make mapping
      counties <- left_join(WA, eviction_poverty, na.rm = TRUE)

      # filters based on reactive inputs
      if(reactive_topic == "Evictions") {
         counties$categories <- cut(counties$Evictions,
                                    breaks = c(1, 5, 20, 50, 100, 200, 300, 500, 1500, 3000),
                                    labels = c("1 < E < 5", "5 < E < 20", "20 < E < 50", "50 < E < 100", "100 < E < 200",
                                               "200 < E < 300", "300 < E < 500", "500 < E < 1500", "1500 < E < 3000"), 
                                    include.lowest = TRUE)
      } else {
         counties$categories <- cut(counties$`Poverty Rate`,
                                    breaks = c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27),
                                    labels = c("1 < P < 3", "3 < P < 6", "6 < P < 9", "9 < P < 12", "12 < P < 15",
                                               "15 < P < 18", "18 < P < 21", "21 < P < 24", "24 < P < 27"), 
                                    include.lowest = TRUE)
      }
      
      plot_title <- paste(reactive_topic, "in Washington (by county) in", sep = " ")
      plot_title <- paste(plot_title, reactive_year, sep = " ")

      plot <- counties %>%
         group_by(group) %>%
         plot_ly(x = ~long, y = ~lat, 
                 color = ~categories, 
                 colors = c('YlOrBr'),
                 showlegend = FALSE,
                 text = ~c(counties$County, counties$reactive_key),
                 hoverinfo = 'text'
                 ) %>%
         add_polygons(line = list(width = 0.4)) %>%
         add_polygons(
            fillcolor = 'transparent',
            line = list(color = 'black', width = 0.5)
         ) %>%
         layout(
            title = plot_title,
            titlefont = list(size = 10),
            xaxis = list(title = "", showgrid = FALSE,
                         zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(title = "", showgrid = FALSE,
                         zeroline = FALSE, showticklabels = FALSE)
         )
      return(plot)
   })
   output$map <- renderPlotly(reactive_map())
   
   reactive_table_info <- reactive({
      reactive_topic <- input$filter
      reactive_year <- input$year
      
      tinfo <- paste("This is a table pertaining to", reactive_topic, sep = " ")
      tinfo <- paste(tinfo, "in Washington State (by county) in", sep = " ")
      tinfo <- paste(tinfo, reactive_year, sep = " ")
      tinfo <- paste0(tinfo, ". It lists the year, county, population, median household income (of the county), and the number of evictions.")
      
      return(tinfo)
   })
   
   output$table_info <- renderText({reactive_table_info()})
   
   reactive_map_info <- reactive({
      reactive_topic <- input$filter
      reactive_year <- input$year
      
      minfo <- paste("This is a map to visualize", reactive_topic, sep = " ")
      minfo <- paste(minfo, "in Washington (at the county level) in", sep = " ")
      minfo <- paste(minfo, reactive_year, sep = " ")
      minfo <- paste(minfo, ". It enables a hover-over feature that will tell you the specific", sep = " ")
      minfo <- paste(minfo, reactive_topic, sep = " ")
      if(reactive_topic == "Evictions") {
         minfo <- paste0(minfo, ". E stands for the number of evictions in that county.
                         If the county is white, then that means that the data on evictions in that 
                         county is NA (insufficient or unavailable due to evictionlab's dataset)")
      } else {
         minfo <- paste0(minfo, ". P stands for the proverty rate (the percent of the population that earns below the poverty line).
                         If the county is white, then that means that the data on poverty rates in
                         that count is NA (insufficient or unavailable due to evictionlab's dataset)")
      }
      
      return(minfo)
   })
   output$map_info <- renderText({reactive_map_info()})
}
shinyServer(my_server)
