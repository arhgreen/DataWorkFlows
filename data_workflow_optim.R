# Load packages----------------------------------------------------------------
library(tidyverse)
library(fs)

# Create vector of csvs--------------------------------------------------------
# useful when you e.g have multiple csv files with the same structure. 
csv_files <- dir_ls(
  path = "data-raw",
  regexp = "csv"
)

# Import data------------------------------------------------------------------
# this code creates a function that takes a given csv file, selects resp. id and
# location, separate the location variable into city and state. e.g 
# Portland, OR is separated into Portland and OR as city and state resp.
# after which a new column named survey_yearis created from the name of the 
#csv file and later removing unwanted words to just the survey year

# DEMOGRAPHICS DATA
import_demographic_data <- function(csv_file){
  read_csv(csv_file) %>% 
    select(respondent_id, location) %>% 
    separate_wider_delim(
      cols = location,
      delim = ", ",
      names = c("city", "state")
    ) %>% 
    mutate(survey_year = csv_file) %>% 
    mutate(survey_year = str_remove(survey_year, "data-raw/survey_data_")) %>% 
    mutate(survey_year = str_remove(survey_year, ".csv")) %>% 
    mutate(survey_year = as.numeric(survey_year))
}
# testing the function
import_demographic_data("data-raw/survey_data_2021.csv")

# Doing the same thing for all the csv files and binding them into one csv file
demographics_data <- map(
  csv_files, import_demographic_data) %>%
  bind_rows()

head(demographics_data)
tail(demographics_data)

# Saving Demographics data as an rds file into the data folder
demographics_data %>% 
  write_rds("data/demographics_data.rds")

# Cleaning untidy variables-----------------------------------------------------
# Since the favorite parts variable in each of the csv file is untidy i.e.
# a respondent can have more than one favorite part then we need to first 
# clean it for easier analysis 
import_favorite_parts_data <- function(csv_file){
  read_csv(csv_file) %>% 
    select(respondent_id, favorite_parts) %>% 
    separate_longer_delim(
      cols = favorite_parts, 
      delim = ", "
    ) %>% 
    rename(favorite_part = favorite_parts) %>% 
    mutate(survey_year = csv_file) %>% 
    mutate(survey_year = str_remove(survey_year, "data-raw/survey_data_")) %>% 
    mutate(survey_year = str_remove(survey_year, ".csv")) %>% 
    mutate(survey_year = as.numeric(survey_year))
}

# testing function
import_favorite_parts_data("data-raw/survey_data_2021.csv")

# Doing the same thing for all the csv files and binding them into one csv file
favorite_parts_data <- map(
  csv_files, import_favorite_parts_data) %>%
  bind_rows()

head(favorite_parts_data)
tail(favorite_parts_data)

# Saving Demographics data as an rds file into the data folder
favorite_parts_data %>% 
  write_rds("data/favorite_parts_data.rds")


# PRE-POST QUESTIONS
import_pre_post_data <- function(csv_file){
  read_csv(csv_file) %>% 
    select(respondent_id, pre_question_1:pre_question_2) %>% 
    pivot_longer(
      cols = -respondent_id
    ) %>% 
    separate_wider_delim(
      cols = name,
      delim = "_",
      names = c("timing", "question", "question_number")
    ) %>% 
    select(-question) %>% 
    mutate(survey_year = csv_file) %>% 
    mutate(survey_year = str_remove(survey_year, "data-raw/survey_data_")) %>% 
    mutate(survey_year = str_remove(survey_year, ".csv")) %>% 
    mutate(survey_year = as.numeric(survey_year))
}

# testing function 
import_pre_post_data("data-raw/survey_data_2021.csv")

# Doing the same for all csv files
pre_post_data <- map(csv_files, import_pre_post_data) %>% 
  bind_rows()

tail(pre_post_data)

pre_post_data %>% 
  write_rds("data/pre_post_data.rds")

# SATISFACTIONS DATA 
import_satisfactions_data <- function(csv_file) {
  read_csv(csv_file) %>% 
    select(respondent_id, contains("satisfaction")) %>% 
    pivot_longer(
      cols = -respondent_id,
      ) %>% 
    separate_wider_delim(
      cols = name,
      delim = "_",
      names = c("question_type", "question", "question_number")
    ) %>% 
    select(-c(question_type, question)) %>% 
    mutate(survey_year = csv_file) %>% 
    mutate(survey_year = str_remove(survey_year, "data-raw/survey_data_")) %>% 
    mutate(survey_year = str_remove(survey_year, ".csv")) %>% 
    mutate(survey_year = as.numeric(survey_year))
}

# testing function
import_satisfactions_data("data-raw/survey_data_2021.csv")

# Binding everything together
satisfaction_data <- map(
  csv_files, import_satisfactions_data) %>% 
  bind_rows()

satisfaction_data %>% 
  write_rds("data/satisfaction_data.rds")

# Examples ----------------------------------------------------------------

# satisfaction_questions |>
#   group_by(question_number) |>
#   summarize(mean_response = mean(value, na.rm = TRUE))

# favorite_parts |>
#   left_join(
#     demographics,
#     join_by(respondent_id)
#   ) |>
#   filter(city == "Portland") |>
#   count(favorite_parts)
