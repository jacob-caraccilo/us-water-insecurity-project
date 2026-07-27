library(tidyverse)
library(glue)

# retrieves Census API key stored in the user's local .Renviron file
census_key <- Sys.getenv("CENSUS_API_KEY")

# stops the script if a Census API key is not found and directs user on what to do
if (census_key == "") {
  stop("Your Census API key was not found. Add it to .Renviron, then restart RStudio.")
}

# downloads variables of interest from the Census and formats them
download_acs_county_data <- function(year) {
  
  # glue() used to make link easily readable and inserts Census API key
  api_url <- glue(
    "https://api.census.gov/data/{year}/acs/acs5?",
    "get=NAME,B19013_001E,B03002_001E,B03002_003E,",
    "B03002_004E,B03002_005E,B03002_006E,B03002_012E",
    "&for=county:*&in=state:*",
    "&key={census_key}",
    "&outputFormat=csv"
  )
  
  # pulls Census data for median income and racial demographics
  read_csv(
    api_url,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    # renames the columns to reflect what the data actually represents rather than their codes
    rename(
      county_name = NAME,
      median_household_income = B19013_001E,
      total_population = B03002_001E,
      nonhispanic_white = B03002_003E,
      nonhispanic_black = B03002_004E,
      nonhispanic_native = B03002_005E,
      nonhispanic_asian = B03002_006E,
      hispanic_latino = B03002_012E
    ) |>
    # eliminates Puerto Rico from then dataframe since we are focusing solely on states
    filter(state != "72") |>
    mutate(
      # formats the Census data to have the same FIPS codes as the water insecurity data
      state_fips = str_pad(state, width = 2, pad = "0"),
      county_fips = str_pad(county, width = 3, pad = "0"),
      # combines the two FIPS codes together to join the water insecurity dataset and the Census data
      geoid = paste0(state_fips, county_fips),
      
      # converts all of these columns from character variables into numerical values
      across(
        c(
          median_household_income,
          total_population,
          nonhispanic_white,
          nonhispanic_black,
          nonhispanic_native,
          nonhispanic_asian,
          hispanic_latino
        ),
        as.numeric
      ),
      
      # assigns each state to one of the four major Census regions (Northeast, Midwest, West, or South)
      region = case_when(
        state_fips %in% c(
          "09", "23", "25", "33", "44", "50",
          "34", "36", "42"
        ) ~ "Northeast",
        
        state_fips %in% c(
          "17", "18", "26", "39", "55",
          "19", "20", "27", "29", "31", "38", "46"
        ) ~ "Midwest",
        
        state_fips %in% c(
          "10", "11", "12", "13", "24", "37",
          "45", "51", "54", "01", "21", "28",
          "47", "05", "22", "40", "48"
        ) ~ "South",
        
        state_fips %in% c(
          "02", "04", "06", "08", "15", "16",
          "30", "32", "35", "41", "49", "53",
          "56"
        ) ~ "West"
      ),
      
      # creates a new column named year and supplies it with the value we give when running the function
      year = year
    ) |>
    # sets all columns to be in final dataframe
    select(
      year,
      geoid,
      county_name,
      state_fips,
      county_fips,
      region,
      total_population,
      median_household_income,
      starts_with("nonhispanic_"),
      hispanic_latino
    )
}

# creates and downloads 2022 and 2023 Census and water insecurity data
census_2022 <- download_acs_county_data(2022)
census_2023 <- download_acs_county_data(2023)

# saves files into the data folder for project
write_csv(census_2022, "data/census_2022.csv")
write_csv(census_2023, "data/census_2023.csv")