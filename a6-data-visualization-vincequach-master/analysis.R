library("ggplot2")
library("dplyr")
library("hexbin")
library("maps")
library("tidyr")
library("plotly")

source("assignment.R")

##############
## Part one ##
##############
just_kt <- filter(emissions, Series.Code == "EN.ATM.CO2E.KT")

alphabetical <- just_kt[order(just_kt$ï..Country.Code), ]
alphabetical <- head(alphabetical, 10)
names <- alphabetical$ï..Country.Code

first_10 <- ggplot(data = alphabetical) + 
   geom_point(mapping = aes(x = ï..Country.Code , y = Most_Recent, color = Most_Recent), na.rm = TRUE) +
   labs(
      title = "Pollution in Kilotons in 2014 or most recent year",
      x = "Country (Code)",
      y = "Pollution (kilotons)",
      color = "CO2 emissions gradient (kt)"
   )
# Lowest KT polluter out of alphabetical 
lowest_a <- arrange(alphabetical, Most_Recent) %>%
   head(1)
lowest_a_country <- lowest_a$ï..Country.Code
lowest_a_pollution <- lowest_a$Most_Recent

# highest KT polluter out of alphabetical
highest_a <- arrange(alphabetical, -Most_Recent) %>%
   head(2)
highest_a_country <- highest_a$ï..Country.Code
highest_a_pollution <- highest_a$Most_Recent

# Data frame of overall 5 lowest polluters (kt)
lowest <- arrange(just_kt, Most_Recent) %>%
   head(5)
lowest_names <- lowest$ï..Country.Code
tuv <- head(lowest, 1)
tuvalu <- tuv$ï..Country.Code
tuvalu_pollution <- tuv$Most_Recent

# Data frame of overall 5 highest polluters (kt)
highest <- arrange(just_kt, -Most_Recent) %>%
   head(5)
highest_names <- highest$ï..Country.Code
ch <- head(highest, 1)
china <- ch$ï..Country.Code
china_pollution <- ch$Most_Recent

difference <- round(china_pollution / tuvalu_pollution, 0)

# Joining the two
lowest_and_highest <- full_join(lowest, highest) %>%
   select(ï..Country.Code, Series.Code, Most_Recent) %>%
   arrange(-Most_Recent)

highest_vs_lowest <- ggplot(lowest_and_highest, aes(ï..Country.Code, Most_Recent, fill = Most_Recent)) +
   geom_bin2d(na.rm = TRUE) +
   labs(
      title = "2d Bin of Top 5 and Bottom 5 Polluters ",
      x = "Country (Code)",
      y = "Pollution (in kilotons)",
      fill = "CO2 emissions gradient (kt)"
   )

lowest_plot <- highest_losest_facet <- ggplot(lowest, aes(ï..Country.Code, Most_Recent, fill = Most_Recent)) +
   geom_bin2d(na.rm = TRUE) +
   labs(   
      title = "2d Bin of the Five Lowest Polluting Countries",
      x = "Country (Code)",
      y = "Pollution (in kilotons)",
      fill = "CO2 emissions gradient (kt)"
   )

highest_plot <- highest_losest_facet <- ggplot(highest, aes(ï..Country.Code, Most_Recent, fill = Most_Recent)) +
   geom_bin2d(na.rm = TRUE) +
   labs(   
      title = "2d Bin of the Five Lowest Polluting Countries",
      x = "Country (Code)",
      y = "Pollution (in kilotons)",
      fill = "CO2 emissions gradient (kt)"
   )

##############
## Part Two ##
##############

globe <- map_data("world")
globe <- mutate(globe, Country_Code = iso.alpha(globe$region, n = 3))
emissions <- rename(emissions, Country_Code = ï..Country.Code)
globe <- left_join(globe, emissions, by = "Country_Code")
global_kt <- filter(globe, Series.Code == "EN.ATM.CO2E.KT")
categorical_key <- cut(global_kt$Most_Recent,
                       breaks = c(10, 1000, 10000, 200000, 600000, 1000000, 11000000),
                       labels = c("10 < P < 1000", "1000 < P < 10,000", "10,000 < P < 200,000", "200,000 < P < 600,000",
                                  "600,000 < P < 1,000,000", "1,000,000 < P < 11,000,000"),
                       include.lowest = TRUE
                       )
global_kt <- mutate(global_kt, factors = categorical_key)
the_world <- ggplot(data = global_kt) +
   coord_quickmap() +
   geom_polygon(aes(x = long, y = lat, group = group, fill = factors)) +
   scale_fill_brewer(palette = "OrRd") +
   labs(
      title = "HeatMap (Choropleth) of the World's CO2 Emissions",
      fill = "Carbon Dioxide Pollution(P) in kilotons",
      x = "Longitude",
      y = "Latitude"
   )

min_kt <- min(global_kt$Most_Recent, na.rm = TRUE)
max_kt <- max(global_kt$Most_Recent, na.rm = TRUE)
kt_countries <- nrow(just_kt)
diff <- max_kt - min_kt
interval_size <- diff / kt_countries
################
## Part Three ##
################
just_pc <- filter(emissions, Series.Code == "EN.ATM.CO2E.PC")
global_pc <- filter(globe, Series.Code == "EN.ATM.CO2E.PC")
# pc is metric tons per capita
pc_key <- cut(global_pc$Most_Recent, 
              breaks = c(0, 1, 5, 10, 16, 26, 46),
              labels = c("0 < P < 1", "1 < P < 5", "5 < P < 10", "10 < P < 16", "16 < P < 26", "26 < P < 46")
              )
global_pc <- mutate(global_pc, factors = pc_key)
relevant_global_pc <- select(global_pc, long, lat, group, region, subregion, Country_Code, Series.Code, Most_Recent, factors)


relevant_global_pc$hover <- with(relevant_global_pc, paste(region, '<br>', "Pollution (metric tons per capita)", Most_Recent))

# black boundaries
borders <- list(color = toRGB("black"), width = 0.3)

# specify map projections/options
options <- list(
   showframe = TRUE,
   showcoastlines = FALSE,
   projection = list(type = 'Mercator')
)

world_pc <- plot_geo(relevant_global_pc) %>%
   add_trace(
      z = ~Most_Recent, 
      color = ~Most_Recent, 
      colors = 'Oranges',
      text = ~region, 
      locations = ~Country_Code, 
      marker = list(line = borders)
   ) %>%
   colorbar(title = 'CO2 Emissions (metric tons per capita)', ticksuffix = '(metric tons per capita)') %>%
   layout(
      title = '2014 World Pollution<br>(Hover for information)',
      geo = options
   )


   
   