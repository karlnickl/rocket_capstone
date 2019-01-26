# Data Preparation for State Abbreviation File

# Read/Write Path Definitions ----

# Wikipedia State Table Source
state_path <- "data/raw/reference_data/state_abbreviations.csv"

# Final Mapping File Destination
destination <- "data/pipeline/reference_data/state_abbreviations_domestic.csv"

# Load Required Libraries and Source Files
library(tidyverse)
source("data_prep_utils.R")

# Read the Wikipedia state abbreviation file  ----
state_abbreviations_dat <- read_wiki_state_table(state_path, TRUE)

# Write the Final Mapping File to CSV
write_csv(state_abbreviations_dat, destination)

rm(state_abbreviations_dat, destination, state_path)
