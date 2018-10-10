#################
#### PART 1 #####
#################

names <- c("Patricia Au", "Bao Dinh", "Anukriti Goyal", "Michelle Ho", "Andrew Kan", "Kishore Vasan")
math_grades <- c(69, 100, 85, 71, 93, 40)
spanish_grades <- c(42, 65, 80, 99, 12, 70)
tas <- data.frame(names, math_grades, spanish_grades, stringsAsFactors = FALSE)
rows <- nrow(tas)
columns <- ncol(tas)
column_names <- colnames(tas)
colnames <- paste(colnames(tas), collapse = ", ")
sentence <- paste("The TA data frame has", rows, sep = " ")
sentence <- paste(sentence, "rows", sep = " ")
sentence <- paste(sentence, "and", sep = " ")
sentence <- paste(sentence, columns, sep = " ")
sentence <- paste(sentence, "cols:", sep = " ")
sentence <- paste(sentence, colnames, sep = " ")
print(sentence)
rownames(tas) <- names
print(tas["Michelle", ])
tas$grade_diff <- tas$math_grades - tas$spanish_grades
tas$better_at_math <- tas$grade_diff > 0
num_better_at_math <- sum(tas$better_at_math)
print(num_better_at_math)
write.csv(tas, file = "grade_data.csv")

#################
#### PART 2 #####
#################

life_expectancy <- read.csv(file = "life_expectancy.csv")
life_expectancy$change <- life_expectancy$le_2013 - life_expectancy$le_1960
num_small_gain <- sum(life_expectancy$change < 5)
print(num_small_gain)
most_improved <- life_expectancy[life_expectancy$change == max(life_expectancy$change), ]$country
print(most_improved)

# Takes in a country name and returns the change in life expectancy
# of the country between 1970 ad 2013
country_change <- function(country) {
   country_change <- life_expectancy[life_expectancy$country == country, "change"] 
   return(country_change)
}
print(country_change("Haiti"))

# Takes in a region name and returns the country that has the lowest
# life expectancy in 2013
lowest_life_exp_in_region <- function(region) {
   min_exp_region <- min(life_expectancy[life_expectancy$region == region, "le_2013"])
   min_exp <- life_expectancy[life_expectancy$le_2013 == min_exp_region, ]$country
   return(min_exp)
}
latin_caribbean_lowest <- lowest_life_exp_in_region("Latin America & Caribbean")
print(latin_caribbean_lowest)

# Takes in two countries (c1 and c2) and compares them based on their 2013 l.e. 
# and how much their l.e. changed between 1960 and 2013
compare_countries <- function(c1, c2) {
   c1_2013 <- life_expectancy[life_expectancy$country == c1, "le_2013"]
   c1_change <- life_expectancy[life_expectancy$country == c1, "change"]
   c2_2013 <- life_expectancy[life_expectancy$country == c2, "le_2013"]
   c2_change <- life_expectancy[life_expectancy$country == c2, "change"]
   country <- c(c1, c2)
   le_2013 <- c(c1_2013, c2_2013)
   change <- c(c1_change, c2_change)
   c1_c2_vector <- data.frame(country, le_2013, change)
   return(c1_c2_vector)
}
us_vs_cuba <- compare_countries("United States", "Cuba")

#################
#### PART 3 #####
#################

Titanic <- read.csv(file = "Titanic")
Titanic <- data.frame(Titanic, stringsAsFactors = FALSE)
is.data.frame(Titanic)
children <- Titanic[Titanic$Age == 'Child', ]
children_num <- sum(children$Freq)
print(children_num)

## Prints the row in the overall Titanic frame that     ##
## had the largest number of people who did not survive ##
most_casualties <- max(Titanic[Titanic$Survived == "No", ]$Freq)
print(Titanic[Titanic$Freq == most_casualties, ])

survival_rate <- function(class) {
   #Create a data frame of just the class
   class_frame <- Titanic[Titanic$Class == class, ]
   
   # Seperate the class frame into adults and kids (respectively)
   adults <- class_frame[class_frame$Age == "Adult", ] # data frame of just adults
   child_frame <- class_frame[class_frame$Age == "Child", ] # data frame of just kids
   
   # Seperate the adults into data frames of each sex
   males <- adults[adults$Sex == "Male", ] # data frame of male adults
   females <- adults[adults$Sex == "Female", ] # data frame of female adults
   
   # Finds the sum of survivors in each respective data frame
   male_alive <- sum(males[males$Survived == "Yes", ]$Freq) # sum of alive males
   female_alive <- sum(females[females$Survived == "Yes", ]$Freq) # sum of alive females
   child_alive <- sum(child_frame[child_frame$Survived == "Yes", ]$Freq) # sum of alive kids
   
   # Total passengers in given class
   male_total <- sum(class_frame[class_frame$Sex == "Male", ]$Freq)
   female_total <- sum(class_frame[class_frame$Sex == "Female", ]$Freq)
   total_passengers <- male_total + female_total
   
   # Percentage of sex over total passengers in class
   percent_male <- round((male_alive / total_passengers) * 100, 0)
   percent_female_and_kids <- round(((female_alive + child_alive) / total_passengers) * 100, 0)
   
   # The output message
   message <- paste("Out of", total_passengers, sep = " ")
   message <- paste(message, "passengers in", sep = " ")
   message <- paste(message, class, sep = " ")
   message <- paste(message, "class,", sep = " ")
   message <- paste(message, percent_male, sep = " ")
   message <- paste(message, "% of men survived and", sep = " ")
   message <- paste(message, percent_female_and_kids, sep = " ")
   message <- paste(message, "% of women and children survived.", sep = " ")
   return(message)
}
print(survival_rate('1st'))
print(survival_rate('2nd'))
print(survival_rate('3rd'))

# The "women and children first" policy seemed to be followed more in 1st and 2nd class, where
# 45% and 36% of women and children survived vs 18% and 5% of men survived. However, in 3rd 
# class, the number was very similar, which seems like the crew's priority was to implement a
# real life class filter, then a sex/age filter :)