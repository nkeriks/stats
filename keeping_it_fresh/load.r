library(reshape2)
library(magrittr)
library(stringr)
library(dplyr)
library(jsonlite)

# utils
# I don't like make.names
make_names <- function(x) {
    gsub('[.]', '_', make.names(x))
}

#sometimes NULL is annoying
extract_if_exists <- function (lst, key) {
    ifelse(key %in% names(lst), lst[[key]], NA)
}

flatten_keys <- function (ul, flatten) {
    # flatten a set of keys in a list
    for (nm in flatten) {
        if (length(ul[[nm]]) > 0) {
            flat <- as.list(unlist(ul[[nm]]))
            names(flat) <- paste(nm, make_names(names(flat)), sep='_')
            ul <- c(ul, flat)
        }
        ul[[nm]] <- NULL
    }
    ul
}


#' Takes a file with one json per line and a function
#' to process each line into a flattened list
#' returns a dataframe
#' This is more easily done with jsonlite::fromJSON(., flatten=TRUE)
#' but I wanted to process some things first, for some reason
json_to_dataframe <- function(filename, row_processor) {

    json_lists <- readLines(filename) %>%
        lapply(jsonlite::fromJSON) %>%
        lapply(row_processor)

    all_keys <- lapply(json_lists, names) %>%
        unlist %>%
        unique

    # seems like this could be less slow
    ans <- lapply(all_keys, function (nm) {
        sapply(json_lists, extract_if_exists, nm)
        }
    )

    names(ans) <- all_keys
    as_data_frame(ans)
}

user_processor <- function (ul) {
    ul$friend_count <- length(ul$friends)
    ul$friends <- NULL
    if (length(ul$elite) > 0) {
        ul$elite_years <- length(ul$elite)
        ul$last_elite <- max(ul$elite)
    } else {
        ul$elite_years <- 0
        ul$last_elite <- NA
    }
    ul$elite <- NULL
    flatten_keys(ul, c('compliments', 'votes'))
}



business_processor <- function (ul) {
    cats <- as.list(rep(TRUE, length(ul$categories)))
    names(cats) <- paste('category', make_names(ul$categories), sep='_')
    ul <- c(ul, cats)
    ul$categories <- paste(ul$categories, collapse=';')
    # hours
    if (length(ul$hours) > 0) {
        ul$days_open <- length(ul$hours)
    }
    flatten_keys(ul, c('hours', 'attributes'))
}


build_checkins <- function() {
    wide <- json_to_dataframe('data/yelp_academic_dataset_checkin.json', 
                              function (ul) flatten_keys(ul, c('checkin_info')))
    wide$type <- NULL
    long <- melt(wide, id.vars='business_id') %>% tbl_df
    names(long) <- c('business_id', 'variable', 'count')
    long %<>% mutate(variable=gsub('checkin_info_X', '', variable))
    time_day <- str_split(long$variable, '_')
    day_names <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
    long %<>% mutate(checkin_time=sapply(time_day, function(x) {as.numeric(x[1])}),
                     checkin_day=sapply(time_day, function(x) {day_names[as.integer(x[2]) + 1]}),
                     variable=NULL)
    long
}



get_restaurant_ids <- function() {
    rid <- read.csv('data/restaurant_ids_to_yelp_ids.csv', na.strings='')
    names(rid)[2:5] <- 'business_id'
    rbind(rid[c(1,2)], rid[c(1,3)], rid[c(1,4)]) %>% 
        filter(!is.na(business_id)) %>% 
        mutate(business_id=as.character(business_id),
               restaurant_id=as.character(restaurant_id)) %>%
        tbl_df
}

load_city_data <- function(csv_file){
  inspections <- read.csv(csv_file, header=TRUE)
  names(inspections) <- c("id", "date", "restaurant_id", "minor", "major", "severe")
  tbl_df(inspections)
}



### load the data
training <- load_city_data("data/train_labels.csv")
submit <- load_city_data("data/SubmissionFormat.csv")

yelp_to_boston <- get_restaurant_ids()

users <- json_to_dataframe('data/yelp_academic_dataset_user.json', user_processor)

business <- json_to_dataframe('data/yelp_academic_dataset_business.json', business_processor) %>%
    left_join(yelp_to_boston, by='business_id')

reviews <- json_to_dataframe('data/yelp_academic_dataset_review.json',
                             function (ul) flatten_keys(ul, c('votes'))) %>%
    left_join(yelp_to_boston, by='business_id')
    
tips <- json_to_dataframe('data/yelp_academic_dataset_tip.json', function (ul) ul) %>%
    left_join(yelp_to_boston, by='business_id')
checkins <- build_checkins() %>%
    left_join(yelp_to_boston, by='business_id')


save(training, submit, users, business, reviews, tips, checkins, file='data_cache.Rdat')
