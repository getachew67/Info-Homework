# survey <- read.csv(file = "data/intro_survey.csv", stringsAsFactors = FALSE)
# library("dplyr")
# library(ggplot2)

# names(survey)
# View(survey)

# View(survey_num)

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
