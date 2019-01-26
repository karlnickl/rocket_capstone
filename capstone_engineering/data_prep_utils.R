# This file contains functions for preparing data.

# Load required libraries  ----
library(tidyverse)  # tidyverse, for data manipulation
library(readxl)  # for loading excel files

# Load and Clean Raw Wikipedia State Abbreviations Table  ----
read_wiki_state_table <- function(path, states_and_fed_dist_only = TRUE) {
  
  # define region_types to include when states_and_fed_dist_only = TRUE
  domestic_types <- c("State", "Federal district")
  
  # define the schema of the wikipedia state table
  schema <- cols(
    state_or_region    = col_character(),
    region_type        = readr::col_factor(NULL),
    ISO                = col_character(),
    ANSI_alpha         = col_character(),
    USPS               = col_character(),
    USCG               = col_character(),
    GPO                = col_character(),
    AP                 = col_character(),
    Otherabbreviations = col_character()
  )
  
  # read the CSV file
  dat <- read_csv(path, col_types = schema)
  
  # Clean up the region_type column
  dat$region_type <- fct_collapse(
    dat$region_type,
    State                     = c("State", "State (Commonwealth)"),
    "Insular Area"            = c("Insular area (Territory)", "Insular areas",
                                  "Insular area (Commonwealth)"),
    Island                    = c("island"),
    Atoll                     = c("atoll", "atoll[c]"),
    "Freely associated state" = c("Freely associated state"),
    "US military mail code"   = c("US military mail code"),
    "Obsolete postal code"    = c("Obsolete postal code[g]",
                                  "Obsolete postal code",
                                  "Obsolete postal code[h]")
  )
  
  # if only including states and federal districts, filter the other rows out
  if (states_and_fed_dist_only == TRUE) {
    dat <- dat %>% filter(region_type %in% domestic_types)
  }
  
  # include a lowercase version of the state or region column
  state_or_region_lower <- str_to_lower(dat$state_or_region)
  dat <- cbind.data.frame(dat, state_or_region_lower)
  
  # return the final dataset
  dat
}

# Read the clean state abbreviations table  ----
read_state_abbreviations <- 
  function(path = paste0("data/pipeline/reference_data/",
                         "state_abbreviations_domestic.csv")) {
    
    schema <- cols(
      state_or_region       = col_character(),
      region_type           = readr::col_factor(NULL),
      ISO                   = col_character(),
      ANSI_alpha            = col_character(),
      USPS                  = col_character(),
      USCG                  = col_character(),
      GPO                   = col_character(),
      AP                    = col_character(),
      Otherabbreviations    = col_character(),
      state_or_region_lower = col_character()
    )
    
    dat <- read_csv(path, col_types = schema)
    
    dat
  }

# Read in the Raw Sales Order Data into memory  ----
read_raw_sales <- 
  function(path = paste0("data/raw/Sales_data_for_",
                         "Capstone_Project_-_Student_Version_-_v3.xlsx")) {
    
    # define the schema (readxl has a different method for col_types, below
    # code commented and retained for posterity's sake.)
    # schema <- cols(
    #   fiscalquartername     = col_character(),
    #   FiscalPeriod          = col_character(),
    #   FiscalYear            = col_integer(),
    #   Company               = col_character(),
    #   Top_Most_BP           = col_character(),
    #   BusinessPartner       = col_character(),
    #   Item_Number           = col_factor(NULL),
    #   Item_Description      = col_character(),
    #   Ship_to_State         = col_factor(NULL),
    #   Ship_to_City          = col_character(),
    #   Quantity              = col_integer(),
    #   Planned_Delivery_Date = col_date("%Y-%m-%d"),
    #   Address_1             = col_character(),
    #   Address_2             = col_character(),
    #   Postal_Code           = col_character(),
    #   Budget_Qty            = col_integer(),
    #   Forecast_Qty          = col_integer(),
    #   Line_of_Business      = col_factor(NULL)
    # )
    
    schema = c("text", "text", "numeric", "text", "text", "text", "text", 
               "text", "text", "text", "numeric", "date", "text", "text", 
               "text", "numeric", "numeric", "text")
    
    # read the excel file
    dat <- read_excel(
      path, sheet = "Processed Fruits", 
      col_names = TRUE, col_types = schema, na = c("", "NULL", "  ", "N/A"))
  }

# Clean City/State Pair (only works for US States and DC). Function  ----
# returns a list of length 3, corresponding to city, state, and international
# status. 
clean_city_state <- function(
  city, state, 
  state_abbrev_path = paste0("data/pipeline/reference_data/",
                             "state_abbreviations_domestic.csv")
) {
  
  # define two character 'state' string pattern
  two_char_pattern <- "^.+\\s+([A-Z]{2})(\\s+.*|$)"
  
  # define a pattern for matching the full state name
  full_state_pattern <- paste0(
    "^.*(alabama|alaska|arizona|arkansas|california|colorado|connecticut|delaw",
    "are|district of columbia|florida|georgia|hawaii|idaho|illinois|indiana|io",
    "wa|kansas|kentucky|louisiana|maine|maryland|massachusetts|michigan|minnes",
    "ota|mississippi|missouri|montana|nebraska|nevada|new hampshire|new jersey",
    "|new mexico|new york|north carolina|north dakota|ohio|oklahoma|oregon|pen",
    "nsylvania|rhode island|south carolina|south dakota|tennessee|texas|utah|v",
    "ermont|virginia|washington|west virginia|wisconsin|wyoming).*$"
  )
  
  # check if state abbreviation table is loaded, and load if not
  if (!exists("state_abbreviations_dat")) {
    state_abbreviations_dat <- read_state_abbreviations(state_abbrev_path)
  }
  
  # classify the state value
  state_class <- if (is.na(state)) "blank" else {
    if (state %in% state_abbreviations_dat$USPS) "dom" else "int"
  }
  
  # classify the city value
  if (is.na(city)) {
    city_class <- "blank"
    city_has_two_char_pattern <- FALSE
    matches_full_state <- FALSE
    
  } else {
    city_has_two_char_pattern <- str_detect(city, two_char_pattern)
    matches_full_state <- FALSE
    
    if (city_has_two_char_pattern) {
      two_char <- str_match(city, two_char_pattern)[1, 2]
      two_char_is_state <- two_char %in% state_abbreviations_dat$USPS
      
      if (two_char_is_state) {
        city_class <- "2_dom"
        state_in_city <- two_char
        
      } else {
        city_class <- "int"
        
      }
      
    } else {
      matches_full_state <- str_detect(str_to_lower(city), full_state_pattern)
      
      if (matches_full_state) {
        city_class <- "full_dom"
        full_state <- str_match(str_to_lower(city), full_state_pattern)[1, 2]
        state_in_city <- state_abbreviations_dat[
          state_abbreviations_dat$state_or_region_lower == full_state, 
          6
          ]
      } else {
        city_class <- "unknown"
      }
      
    }
  }
  
  # define the final outputs
  if (state_class == "dom" | state_class == "int") {
    state_clean <- state
  } else if (state_class == "blank" & city_has_two_char_pattern) {
    state_clean <- state_in_city
  } else if (state_class == "blank" & matches_full_state) {
    state_clean <- state_in_city
  } else {
    state_clean <- NA
  }
  
  # note that city will not currently "clean". If people want the cities cleaned
  # I can put work in, but unless anyone wants city level data I'll leave the
  # returned city value equal to the inputted city value. Recommend replacing
  # the city column in your code anyway to avoid having to make modifications if
  # a change is implemented
  city <- city
  dom_int <- if (state_class == "dom" | city_class %in% c("2_dom", "full_dom")) {
    "domestic"
  } else "unknown"
  
  # assemble
  output <- c(city, state_clean, dom_int)
  
  output
}

# Write the clean order-level dataset
write_clean_sales_data <- function(
  df, 
  dest = "data/clean/clean_tomato_bag_sales.csv"
) {
  write_csv(df, dest)
}

# Read the Cleaned and integrated Sales Data  ----
read_clean_sales <- function(path = "data/clean/clean_tomato_bag_sales.csv") {
  
  month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  
  schema <- cols(
    fiscalquartername          = col_character(),
    FiscalPeriod               = col_character(),
    FiscalYear                 = col_integer(),
    Company                    = col_character(),
    Top_Most_BP                = col_character(),
    BusinessPartner            = col_character(),
    Item_Number                = readr::col_factor(NULL),
    Item_Description           = col_character(),
    Ship_to_State              = readr::col_factor(NULL),
    Ship_to_City               = col_character(),
    Quantity                   = col_number(),
    Planned_Delivery_Date      = col_datetime(),
    Address_1                  = col_character(),
    Address_2                  = col_character(),
    Postal_Code                = col_character(),
    Budget_Qty                 = col_number(),
    Forecast_Qty               = col_number(),
    Line_of_Business           = readr::col_factor(NULL),
    domestic_or_international  = readr::col_factor(NULL),
    planned_delivery_year      = col_integer(),
    planned_delivery_month     = readr::col_factor(levels = month_levels,
                                                   ordered = TRUE),
    planned_delivery_month_num = col_integer(),
    planned_delivery_day       = col_integer()
  )
  
  dat <- read_csv(path, col_types = schema)
  
  dat$Quantity <- as.integer(dat$Quantity)
  
  dat
}
