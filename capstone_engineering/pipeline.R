# This script is designed to generate the full pipeline of datasets. It should
# be considered the master reference for dependencies, etc.

# Load Required Dependencies  --------------------------------------------------
library(tidyverse)  # for data manipulation and graphics
library(readxl)  # for manipulating Excel files
source("data_prep_utils.R")  # project function library

# Prepare Reference Data  ------------------------------------------------------

# Prepare State Abbreviations
source("generate_state_abbreviations.R")

# Prepare Clean Datasets  ------------------------------------------------------

# Create the integrated sales/order dataset
source("generate_integrated_sales_data.R")

# Clean-Up  --------------------------------------------------------------------
rm(list = ls())