library("httr")
library("jsonlite")
library("dplyr")

source("apiKey.R")

# QUESTIONS TO ASK MICHELLE #
# when I do topic <- "education" in the index.Rmd and representative_id <- member$id it won't work unless
#     I assign it the explicit value of "education" and "S000510" in it in assignment.R
# What to do when there is no phone number
# left_joining goes from 20 to 114

base_uri <- "https://api.propublica.org/congress/v1/"

# Steps to pass through whatever topic of interest
my_topic <- "education" # From the index.Rmd file
topic_uri <- paste0(base_uri, "bills/subjects/")
topic_uri <- paste0(topic_uri, my_topic)
topic_uri <- paste0(topic_uri, ".json")

# Get the bill data
response <- GET(topic_uri, add_headers('X-API-Key' = api_key))
education_body <- content(response, "text")
parsed_education_body <- fromJSON(education_body)
recent_education_bills <- data.frame(parsed_education_body$results, stringsAsFactors = FALSE)
recent_education_bills <- flatten(recent_education_bills)

# Get Representative data
members <- GET(paste0(base_uri, "members/house/WA/9/current.json"), 
               add_headers('X-API-Key' = api_key))
members_body <- content(members, "text")
parsed_members_body <- fromJSON(members_body)
member <- data.frame(parsed_members_body$results, stringsAsFactors = FALSE)
member <- flatten(member)
rep_string <- paste0(base_uri, "members/")
rep_string<- paste0(rep_string, member$id)
rep_string<- paste0(rep_string, ".json")
my_rep <- GET(rep_string, add_headers('X-API-Key' = api_key))
my_rep_body <- content(my_rep, "text")
parsed_my_rep <- fromJSON(my_rep_body)
rep <- data.frame(parsed_my_rep$results, stringsAsFactors = FALSE)
rep <- flatten(rep)

# URI String flexible for different members to get their sponsored bills
member_sponsored_uri <- paste0(base_uri, "members/")
member_sponsored_uri <- paste0(member_sponsored_uri, "S000510")
member_sponsored_uri <- paste0(member_sponsored_uri, "/bills/introduced.json")

# URI String flexible for different members to get their COsponsored bills
member_cosponsored_uri <- paste0(base_uri, "members/")
member_cosponsored_uri <- paste0(member_cosponsored_uri, "S000510")
member_cosponsored_uri <- paste0(member_cosponsored_uri, "/bills/cosponsored.json")
member_cosponsored_uri

# Gets bill sponsored by that member
member_sponsored <- GET(member_sponsored_uri, add_headers('X-API-Key' = api_key))
member_sponsored_body <- content(member_sponsored, "text")
parsed_member_sponsored <- fromJSON(member_sponsored_body)
sponsored_bills <- data.frame(parsed_member_sponsored$results, stringsAsFactors = FALSE)
sponsored_bills <- data.frame(sponsored_bills$bills, stringsAsFactors = FALSE)
sponsored_bills <- flatten(sponsored_bills)
sponsored_bills <- head(sponsored_bills, 10)

# Gets bills cosponsored by that member
member_cosponsored <- GET(member_cosponsored_uri, add_headers('X-API-Key' = api_key))
member_cosponsored_body <- content(member_cosponsored, "text")
parsed_member_cosponsored <- fromJSON(member_cosponsored_body)
cosponsored_bills <- data.frame(parsed_member_cosponsored$results, stringsAsFactors = FALSE)
cosponsored_bills <- data.frame(cosponsored_bills$bills, stringsAsFactors = FALSE)
cosponsored_bills <- flatten(cosponsored_bills)
cosponsored_bills <- head(cosponsored_bills, 10)

# Gets the recent votes of my representative
# String to make it dynamic for different reps
rep_vote_string <- paste0(base_uri, "members/")
rep_vote_string <- paste0(rep_vote_string, rep$member_id)
rep_vote_string <- paste0(rep_vote_string, "/votes.json")

my_rep_votes <- GET(rep_vote_string, add_headers('X-API-Key' = api_key))
my_rep_votes_body <- content(my_rep_votes, "text")
parsed_rep_votes <- fromJSON(my_rep_votes_body)
rep_votes <- data.frame(parsed_rep_votes$results, stringsAsFactors = FALSE)
rep_votes <- data.frame(rep_votes$votes, stringsAsFactors = FALSE)
rep_votes <- flatten(rep_votes)

# Gets the recent votes of the entire HoR
house_votes <- GET(paste0(base_uri, "house/votes/recent.json"), add_headers('X-API-Key' = api_key))
house_votes_body <- content(house_votes, "text")
parsed_house_votes <- fromJSON(house_votes_body)
votes <- data.frame(parsed_house_votes$results, stringsAsFactors = FALSE)
votes <- flatten(votes)


