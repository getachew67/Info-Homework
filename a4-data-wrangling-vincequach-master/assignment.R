# install.packages("dplyr")
library("dplyr")

#################
#### PART 1 #####
#################

drinking <- read.csv(file = "data/any_drinking.csv", stringsAsFactors = FALSE)
is.data.frame(drinking)

# Where females drink more than males
drinking_2012 <- select(drinking, state, location, both_sexes_2012, females_2012, males_2012)
m_f_diff_2012 <- mutate(drinking_2012, difference = males_2012 - females_2012)
females_more <- filter(m_f_diff_2012, females_2012 > males_2012)
female_more_message <- paste("Females drank more than males in", nrow(females_more), sep = " ")
female_more_message <- paste(female_more_message, "locations.", sep = " ")
print(female_more_message)

# Where males and females drink most similarly 
closest_ratio <- filter(m_f_diff_2012, difference == min(difference))
closest_location <- select(closest_ratio, location)
closest_state <- select(closest_ratio, state)
closest_mf <- paste("Females and males drink near equally in", closest_location, sep = " ")
closest_mf <- paste(closest_mf, ",", sep = "")
closest_mf <- paste(closest_mf, closest_state, sep = " ")
print(closest_mf)

# Finds the states with the highest and lowest combined drinking rates
just_states <- select(drinking_2012, state, both_sexes_2012, females_2012, males_2012)
min_max_state <- select(just_states, state, both_sexes_2012)
highest_drinking_state <- filter(min_max_state, both_sexes_2012 == max(both_sexes_2012))
print(highest_drinking_state)
lowest_drinking_state <- filter(min_max_state, both_sexes_2012 == min(both_sexes_2012))
print(lowest_drinking_state)

# The difference between the two states 
highest_combined <- summarize(just_states, max = max(both_sexes_2012))
lowest_combined <- summarize(just_states, min = min(both_sexes_2012))
difference <- highest_combined - lowest_combined
difference_sentence <- paste("Range in drinking rate was", difference, sep = " ")
difference_sentence <- paste(difference_sentence, "%", sep = "")
print(difference_sentence)

#################
#### PART 2 #####
#################

binge_drinking <- read.csv(file = "data/binge_drinking.csv", stringsAsFactors = FALSE)
just_states <- filter(binge_drinking, state != "National")
just_counties <- filter(just_states, state != location)
nrow(binge_drinking) - nrow(just_counties) == 52 # states and 2 D.C should have a diff of 52
# adds cols. of the difference in drinking between all sexes b/t 2002 and 2012 in just one line.
just_counties <- mutate(just_counties,
                       diff_males = males_2012 - males_2002,
                       diff_females = females_2012 - females_2002,
                       diff_both = both_sexes_2012 - both_sexes_2002
                 )
summarize(just_counties, mean = mean(both_sexes_2012))

# Produces a data frame that states the highest and lowest number of binge drinking 
#  for both sexes per state, including DC
min_max_per_state <- just_states %>%
   group_by(state) %>%
   summarize(
      max = max(both_sexes_2012),
      min = min(both_sexes_2012)
   )
write.csv(min_max_per_state, file = "state_binge_drinking.csv")

total_counties <- nrow(just_counties) # total # of counties

# Finds how many counties had an increase of male binge drinking between 2002 and 2012
males_increase_binging <- filter(just_counties, diff_males > 0)
num_males_increase <- nrow(males_increase_binging)
male_increase_percentage <- round((num_males_increase / total_counties) * 100, 0)
male_message <- paste("Male rate of binge drinking increased in", male_increase_percentage, sep = " ")
male_message <- paste(male_message, "% of counties.", sep = "")
print(male_message)

# Finds how many counties had an increase of binge drinking between 2002 and 2012
females_increase_binging <- filter(just_counties, diff_females > 0)
num_females_increase <- nrow(females_increase_binging)
female_increase_percentage <- round((num_females_increase / total_counties) * 100, 0)
female_message <- paste("Female rate of binge drinking increased in", female_increase_percentage, sep = " ")
female_message <- paste(female_message, "% of counties.", sep = "")
print(female_message)

# Finds how many counties had an increase of female binge drinking but a decrease in male binge drinking
female_increase_male_decrease <- filter(females_increase_binging, diff_males < 0)
num_f_increase_m_decrease <- nrow(female_increase_male_decrease)
f_increase_m_decrease_percentage <- round((num_f_increase_m_decrease / total_counties) * 100, 0)
f_increase_m_decrease_message <- paste("Female binge drinking increased and male binge drinking decreased in",
                                       f_increase_m_decrease_percentage, sep = " ")
f_increase_m_decrease_message <- paste(f_increase_m_decrease_message, "% of counties.", sep = "")
print(f_increase_m_decrease_message)

# Finds which state had the largest median increase in male binge drinking
largest_male_median <- group_by(just_counties, state) %>%
   summarize(
      median = median(diff_males)
   ) %>%
   filter(median == max(median)) %>%
   select(state, median)
print(largest_male_median)

# Finds which state had the largest median increase in female binge drinking when female binge
#  drinking increased and male binge drinking decreased
largest_female_median <- group_by(female_increase_male_decrease, state) %>%
   summarize(
      median =  median(diff_females),
      median_both = median(diff_both)
   ) %>%
   filter(median == max(median)) %>%
   select(state, median, median_both)
print(largest_female_median)

#################
#### PART 3 #####
#################

colnames(binge_drinking) <- paste("binge_", colnames(binge_drinking), sep = "")
colnames(drinking) <- paste("any_", colnames(drinking), sep = "")

all_drinking <- left_join(drinking, binge_drinking, by = c("any_state" = "binge_state", "any_location" = "binge_location"), all.x = TRUE, all.y = TRUE)
no_ntl <- filter(all_drinking, any_state != "National")
all_just_states <- filter(no_ntl, any_state == any_location)
all_just_counties <- filter(no_ntl, any_state != any_location)

# The average rate of non-binging in 2012
summarize(all_just_counties, mean = mean(any_both_sexes_2012 - binge_both_sexes_2012))

# A dataframe of the state with the smallest amount of binging in 2012, including the difference using state numbers
min_non_binging_state <- mutate(all_just_states, bingers_non_bingers_difference = (any_both_sexes_2012 - binge_both_sexes_2012)) %>%
   filter(bingers_non_bingers_difference == min(any_both_sexes_2012 - binge_both_sexes_2012)) %>%
   select(any_state, any_both_sexes_2012, binge_both_sexes_2012, bingers_non_bingers_difference)
min_non_binging_state

# A dataframe of the state with the smallest amount of binging in 2012, calculated using county numbers
just_one_state <- select(all_just_counties, any_state, any_both_sexes_2012, binge_both_sexes_2012) # to join back in once it's summarized
minimum_using_counties <- mutate(all_just_counties, non_bingers = (any_both_sexes_2012 - binge_both_sexes_2012)) %>%
   group_by(any_state) %>%
   summarize(mean_diff = mean(non_bingers)) %>%
   left_join(just_one_state, by = c("any_state" = "any_state")) %>%
   filter(mean_diff == min(mean_diff)) %>%
   filter(binge_both_sexes_2012 == min(binge_both_sexes_2012)) %>%
   select(any_state, any_both_sexes_2012, binge_both_sexes_2012, mean_diff)
minimum_using_counties

# The state where the highest percent of total drinkers are binge drinkers (ie. they go hard or they go home)
highest_percentage_bingers <- mutate(all_just_states, percent_of_bingers = ((binge_both_sexes_2012 / any_both_sexes_2012) * 100)) %>%
   filter(percent_of_bingers == max(percent_of_bingers)) %>%
   select(any_state)
highest_percentage_bingers

# Takes in a state and a year, and creates a dataframe of only the state, its counties, and 6 columns
# pertaining to the number of regular drinkers, male, female, and both, and binge drinkers of the same
# categories.
export_state_year <- function(state_name, year) {
   # Turns the state_name argument into a suitable non-standard object
   # that represents the state to be used in a filter
   state_name_sym <- rlang::sym(state_name)
   
   # Turns the year into a suitable non-standard deviation object
   # that represents the any_both column for that year
   any_both <- paste("any_both_sexes_", year, sep = "")
   any_both_sym <- rlang::sym(any_both)
   
   # Turns the year into a suitable non-standard deviation object
   # that represents the any_males column for that year
   any_males <- paste("any_males_", year, sep = "")
   any_males_sym <- rlang::sym(any_males)
   
   # Turns the year into a suitable non-standard deviation object
   # that represents the any_females column for that year
   any_females <- paste("any_females_", year, sep = "")
   any_females_sym <- rlang::sym(any_females)
   
   # Turns the year into a suitable non-standard deviation object
   # that represents the binge_both column for that year
   binge_both <- paste("binge_both_sexes_", year, sep = "")
   binge_both_sym <- rlang::sym(binge_both)
   
   # Turns the year into a suitable non-standard deviation object
   # that represents the binge_males column for that year
   binge_males <- paste("binge_males_", year, sep = "")
   binge_males_sym <- rlang::sym(binge_males)
   
   # Turns the year into a suitable non-standard deviation object
   # that represents the binge_females column for that year
   binge_females <- paste("binge_females_", year, sep = "")
   binge_females_sym <- rlang::sym(binge_females)
   
   # Turns the state_name and year arguments into a string to be used
   # for the file name
   file_name <- paste("drinking_", state_name, sep = "")
   file_name <- paste(file_name, year, sep = "")
   file_name <- paste(file_name, ".csv", sep = "")
   
   # Selects only the state's rows
   state_frame <- filter(all_drinking, any_state == state_name_sym) %>%
      select(any_state, any_location, !!any_both_sym, !!any_males_sym, !!any_females_sym, 
                         !!binge_both_sym, !!binge_males_sym, !!binge_females_sym) %>%
      arrange(!!any_both_sym)
   write.csv(state_frame, file = file_name)
}

# Execution of function
export_state_year("Washington", "2011")
export_state_year("Oregon", "2007")

#################
#### PART 4 #####
#################

# Takes in a state name and a year in order to find what county had the highest ratio of 
# binge drinkers to all drinkers and then publicly shames them for the alcoholism.
find_shameful_drinkers_county <- function(state_name, year) {
   state_name_sym = rlang::sym(state_name)
   
   binge_year <- paste("binge_both_sexes_", year, sep = "")
   binge_year_sym <- rlang::sym(binge_year)
   
   any_year <- paste("any_both_sexes_", year, sep = "")
   any_year_sym <- rlang::sym(any_year)
   
   # Using the binge and any drinker numbers in the year, create a column of the ratio
   all_drinking_and_ratio <- mutate(all_drinking, ratio_of_bingers = (!!binge_year_sym / !!any_year_sym)) %>%
      filter(any_state == state_name_sym) %>%
      filter(ratio_of_bingers == max(ratio_of_bingers)) %>%
      select(any_location)
   
   message <- paste("In", year, sep = " ")
   message <- paste(message, ", the most shameful drinkers in", sep = "")
   message <- paste(message, state_name, sep = " ")
   message <- paste(message, "resided in", sep = " ")
   message <- paste(message, all_drinking_and_ratio, sep = " ")
   message <- paste(message, ". Go hard or go home; I guess they didn't want to go home.", sep = " ")
   return(message)
}

find_shameful_drinkers_county("Washington", "2005")
find_shameful_drinkers_county("California", "2006")
