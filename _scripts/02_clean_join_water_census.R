library(tidyverse)

# loads in water insecurity and census data for both years
water_2022 <- read_csv("data/water_insecurity_2022.csv")
water_2023 <- read_csv("data/water_insecurity_2023.csv")
census_2022 <- read_csv("data/census_2022.csv")
census_2023 <- read_csv("data/census_2023.csv")

# removes Puerto Rico from the water insecurity datasets
water_2022 <- water_2022 |>
  filter(!str_starts(geoid, "72"))
water_2023 <- water_2023 |>
  filter(!str_starts(geoid, "72"))

# removes Bartow County, GA, Lafourche Parish, LA, and Harrison County, TX from
# the water insecurity datasets for whatever reason, these two county / county
# equivalent areas were in the dataset despite lacking plumbing data
water_2022 <- water_2022 |>
  filter(
    !name %in% c(
      "Lafourche Parish, Louisiana",
      "Bartow County, Georgia",
      "Harrison County, Texas"
    )
  )
water_2023 <- water_2023 |>
  filter(
    !name %in% c(
      "Lafourche Parish, Louisiana",
      "Bartow County, Georgia",
      "Harrison County, Texas"
    )
  )

# combines the 2022 water insecurity and Census datasets together based on rows
# where geoid matches, which is the county identifier
water_census_2022 <- left_join(
  water_2022,
  census_2022 |>
    select(-year), # removes year column from census data to avoid two year columns
  by = "geoid"
)

# combines the 2023 water insecurity and Census datasets together based on rows
# where geoid matches, which is the county identifier
water_census_2023 <- left_join(
  water_2023,
  census_2023 |>
    select(-year), # removes year column from census data to avoid two year columns
  by = "geoid"
)

# calculates percent of each demographic in each county and removes the raw 
# demographic totals for 2022 data
water_census_2022 <- water_census_2022 |>
  mutate(
    pct_nonhispanic_white = 100 * nonhispanic_white / total_pop,
    pct_nonhispanic_black = 100 * nonhispanic_black / total_pop,
    pct_nonhispanic_native = 100 * nonhispanic_native / total_pop,
    pct_nonhispanic_asian = 100 * nonhispanic_asian / total_pop,
    pct_hispanic_latino = 100 * hispanic_latino / total_pop
  ) |>
  select(
    -nonhispanic_white,
    -nonhispanic_black,
    -nonhispanic_native,
    -nonhispanic_asian,
    -hispanic_latino,
    -county_name
  )

# calculates percent of each demographic in each county and removes the raw 
# demographic totals for 2023 data
water_census_2023 <- water_census_2023 |>
  mutate(
    pct_nonhispanic_white = 100 * nonhispanic_white / total_population,
    pct_nonhispanic_black = 100 * nonhispanic_black / total_population,
    pct_nonhispanic_native = 100 * nonhispanic_native / total_population,
    pct_nonhispanic_asian = 100 * nonhispanic_asian / total_population,
    pct_hispanic_latino = 100 * hispanic_latino / total_population
  ) |>
  select(
    -nonhispanic_white,
    -nonhispanic_black,
    -nonhispanic_native,
    -nonhispanic_asian,
    -hispanic_latino,
    -county_name
  )

# renames both population columns to include what population estimate source
# they're originating from
water_census_2022 <- water_census_2022 |>
  rename(
    acs1_total_population = total_pop,
    acs5_total_population = total_population
  )

# renames both population columns to include what population estimate source
# they're originating from
water_census_2023 <- water_census_2023 |>
  rename(
    acs1_total_population = total_pop,
    acs5_total_population = total_population
  )

# creates new csv files out of the previously created dataframes and saves them
# in the data folder
write_csv(water_census_2022, "data/water_census_2022.csv")
write_csv(water_census_2023, "data/water_census_2023.csv")