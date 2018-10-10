# install.packages("stringr")
library("stringr")
library("styler")
library("lintr")

#################
#### PART 1 #####
#################

my_age <- 21
my_name <- "Vince"
# Takes in a string "name" and a number "age" and returns a 
# sentence that introduces the person and their age
make_introduction <- function(name, age) {
  intro <- paste("Hello, my name is", name, sep = " ")
  intro <- paste(intro, "and I'm", sep = " ")
  intro <- paste(intro, age, sep = " ")
  intro <- paste(intro, "years old.", sep = " ")
  return(intro)
}
my_intro <- make_introduction(my_name, my_age)
print(my_intro)
casual_intro <- sub("Hello, my name is", "Hey, I'm", my_intro)
print(casual_intro)
capital_intro <- str_to_title(my_intro)
print(capital_intro)
intro_e_count <- str_count(my_intro, "e")
print(intro_e_count)

#################
#### PART 2 #####
#################

books <- c("Ender's game", "In Cold Blood", "Technical Foundations of Informatics", "Harry Potter", "To Kill a Mockingbird", "Ender's Shadow")
print(books)
indices <- c(1, 2, 3)
top_three_books <- books[indices]
print(top_three_books)
book_reviews <- paste(books, "is a great read!", sep = " ")
print(book_reviews)
# Takes in a vector of "books" and a number "index" as paramaters.
# Removes the element at that index from the vector and returns the 
# vector.
remove_book <- function(books, index) {
  all_but_index <- books[-index]
  return(all_but_index)
}
books_without_four <- remove_book(books, 4)
print(books_without_four)
length_goal <- nchar(books) > 15
long_titles <- books[length_goal]
print(long_titles)

#################
#### PART 3 #####
#################

numbers <- 1:201
squared_numbers <- numbers ^ 2
squared_mean <- mean(squared_numbers)
print(squared_mean)
squared_median <- median(squared_numbers)
print(squared_median)
# Looks to see if a value in the vector "numbers" is a square
# by comparing it to the rounded value 
is_square <- (sqrt(numbers) == round(sqrt(numbers), 0))
squares <- numbers[is_square]
print(squares)

#################
#### PART 4 #####
#################

summer_break <- as.Date("2018-06-09")
today <- Sys.Date()
days_to_break <- summer_break - today
print(days_to_break)
# Takes in a string "name", a number "age", and a birth-date "date" and
# creates a message using the "make_introduction" function from above
# but also adds in a string that says when their next birthday is and
# how old they'll be on that day, from the current day. If their birthday
# has already passed then it goes to the next year. 
make_birthday_intro <- function(name, age, date) {
   message <- make_introduction(name, age)
   message <- paste(message, "In", sep = " ")
   days_until_bday <- date - today
   if (days_until_bday >= 0) {
      message <- paste(message, days_until_bday, sep = " ")
   } else {
      message <- paste(message, days_until_bday + 365, sep = " ")
   }
   message <- paste(message, "days I'll be")
   message <- paste(message, age + 1, sep = " ")
   return(message)
}
my_bday_intro <- make_birthday_intro("Vince", 21, as.Date("2018-01-20"))
print(my_bday_intro)