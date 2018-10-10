# survey <- read.csv(file = "data/intro_survey.csv", stringsAsFactors = FALSE)
library("dplyr")


## Part One ##


# calculates the average programming experience of the class
average_programming_exp <- summarize(survey, average = median(programming_exp))

# calculates min and max amount of coffee drinks people drank a day
max_coffee_drinks <- summarize(survey, max = max(coffee_cups))
min_coffee_drinks <- summarize(survey, min = min(coffee_cups))

# calulates percent of people who had an "intermediate" (aka put a 3/4) amount of experience with
# command line, version control, markdown, r language, and web programming
int_of_cl_exp <- filter(survey, cli_exp == 3)
percent_int_of_cl_exp <- round((nrow(int_of_cl_exp) / nrow(survey)) * 100)

int_of_vcs_exp <- filter(survey, vcs_exp == 3)
percent_int_of_vcs_exp <- round((nrow(int_of_vcs_exp) / nrow(survey)) * 100)

int_of_md_exp <- filter(survey, md_exp == 3)
percent_int_of_md_exp <- round((nrow(int_of_md_exp) / nrow(survey)) * 100)

int_of_r_exp <- filter(survey, r_exp == 3)
percent_int_of_r_exp <- round((nrow(int_of_r_exp) / nrow(survey)) * 100)

int_of_web_exp <- filter(survey, web_exp == 3)
percent_int_of_web_exp <- round((nrow(int_of_web_exp) / nrow(survey)) * 100)

# calculates percent of people who had never used (aka put a 0/4)
# command line, version control, markdown, r language, and web programming

no_cl_exp <- filter(survey, cli_exp == 0)
percent_no_cl_exp <- round((nrow(no_cl_exp) / nrow(survey)) * 100)

no_vcs_exp <- filter(survey, vcs_exp == 0)
percent_no_vcs_exp <- round((nrow(no_vcs_exp) / nrow(survey)) * 100)

no_md_exp <- filter(survey, md_exp == 0)
percent_no_md_exp <- round((nrow(no_md_exp) / nrow(survey)) * 100)

no_r_exp <- filter(survey, r_exp == 0)
percent_no_r_exp <- round((nrow(no_r_exp) / nrow(survey)) * 100)

no_web_exp <- filter(survey, web_exp == 0)
percent_no_web_exp <- round((nrow(no_web_exp) / nrow(survey)) * 100)

# calculates percent of people in born in Washington
born_in_wa <- filter(survey, washington_born == "Yes")
percent_born_in_wa <- round((nrow(born_in_wa) / nrow(survey)) * 100)


## Part 4##
survey <- read.csv(file = "data/intro_survey.csv", stringsAsFactors = FALSE)
library("dplyr")
library(ggplot2)

# coffee consumption and web experience
coffee_web <- group_by(survey, coffee_cups) %>%
  summarise(mean = mean(web_exp))

coffee_r <- group_by(survey, coffee_cups) %>%
  summarise(mean = mean(r_exp))

coffee_cons <- left_join(coffee_web, coffee_r, by = c("coffee_cups"))

coffee_programming <- ggplot(data = coffee_cons) +
  geom_line(mapping = aes(x = coffee_cups, y = mean.y, colour = "r_exp")) +
  geom_line(mapping = aes(x = coffee_cups, y = mean.x, colour = "web_exp")) +
  labs(
    title = "Coffee consumption and web programming experience",
    x = "Coffee Consumption (cups)",
    y = "Experience with programming (level of familiarity)",
    color = "Cup(s) of coffee"
  )

# Calculate the amount of coffee consumption
# by people with the highest average programming experience
top_prog_coffee <- filter(coffee_web, mean == max(mean)) %>%
  select(coffee_cups)

# animal preference and r experience
pet_pref_prog <- filter(survey, pet_preference %in% c(
  "dog person",
  "cat person", "fish person",
  "bird person"
))

pet_pref_programming <- ggplot(data = pet_pref_prog) +
  geom_bar(mapping = aes(x = programming_exp, fill = pet_preference)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Pet Preference and Programming Experience",
    x = "Programming experience (scale 1 -5)",
    y = "Number of people",
    fill = "Type of pet"
  )

# calculate the biggest number of dog lovers
# amond people with various levels of programming experience
max_dog_lover <- group_by(pet_pref_prog, programming_exp) %>%
  filter(pet_preference == "dog person") %>%
  filter(programming_exp == 4) %>%
  nrow()


### Part 3 ###

prog_exp_graph <- ggplot(data = survey) +
  geom_bar(mapping = aes(x = programming_exp, fill = "steelblue",
                         color = vcs_exp)) +
  geom_point(aes(x = programming_exp, y = vcs_exp * 10,
                 color = vcs_exp, size = 8)) +
  scale_fill_brewer(palette="Dark2") +
  coord_flip() +
  labs(
    title = "Programming Experience",
    x = "Programming experience (scale 1-5)",
    y = "Number of people",
    color = "vcs_exp * 10",
    size = "dot size"
  )

vcs_graph <- ggplot(data = survey) +
  geom_bar(mapping = aes(x= vcs_exp, fill = "steelblue")) +
  coord_flip() +
  labs(
    title = "Version Control Experience",
    x = "Version control experience (scale 1-3)",
    y = "Number of people",
    fill = "Color"
  )
