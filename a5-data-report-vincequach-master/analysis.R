library("httr")
library("jsonlite")
library("dplyr")

source("assignment.R")

#####################
# Part one analysis #
#####################

sorted_bills <- arrange(recent_education_bills, desc(introduced_date))
most_recent <- head(sorted_bills, 10)
most_recent$legislators <- paste(most_recent$sponsor_name, most_recent$sponsor_state, sep = ", ")
most_recent$legislators <- paste(most_recent$legislators, most_recent$sponsor_party, sep = ", ")
new_most_recent <- select(most_recent, bill_id, short_title, legislators, active, congressdotgov_url)
new_col <- c('Unique Identifier', 'Bill Name', 'Legislator(s)', 'Is It A Law?', 'More Information')
colnames(new_most_recent) <- new_col

#####################
# Part two analysis #
#####################

my_bill <- "hr2407-114"
filter(recent_education_bills, bill_id == my_bill)
if(rep$current_party == "D") {
   opposite_party <- "Republicans"
} else if (rep$current_party == "R") {
   opposite_party <- "Democrats"
} else {
   opposite_party <- "D and R"
}

#######################
# Part three analysis #
#######################

# Joins the two tables of sponsored and cosponsored bills
sponsored_and_cosponsored <- full_join(sponsored_bills, cosponsored_bills, by = c("short_title", "bill_id"))
sponsored_and_cosponsored <- select(sponsored_and_cosponsored, short_title, bill_id)
new_col_names <- c('Bill Name', 'Bill ID Number')
colnames(sponsored_and_cosponsored) <- new_col_names
sponsored_and_cosponsored

# To see the percentage of party votes
rep_v <- select(rep_votes, bill.bill_id, position) %>%
   rename(bill_id = bill.bill_id)
house <- select(votes, votes.bill.bill_id, votes.republican.majority_position) %>%
   rename(bill_id = votes.bill.bill_id)
all_votes <- rep_v %>% 
   mutate(house$votes.republican.majority_position)
matching_votes <- summarize(all_votes, sum = sum(position == house$votes.republican.majority_position))
matching_percentage <- round(matching_votes / nrow(all_votes) * 100, 2)