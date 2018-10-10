library("ggplot2")
library("dplyr")
library("tidyr")

# Reading in all relevant .csv files
emissions <- read.csv(file = "data/WDI_emissions_Data.csv", stringsAsFactors = FALSE)
emissions_definitions <- read.csv(file = "data/WDI_emissions_Definition and Source.csv", stringsAsFactors = FALSE)
selected <- read.csv(file = "data/WDI_selected_Data.csv", stringsAsFactors = FALSE)
selected_definitions <- read.csv(file = "data/WDI_selected_Definition and Source.csv", stringsAsFactors = FALSE)
