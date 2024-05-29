# load libraries
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)


# Import ------------------------------------------------------------------

source("00_Data_Pipeline.R")



# table of contents
# - document how many entries are missing, and what the time distribution is
#       i.e. how many months back is the cutoff for reliable data
# - run/load cleaned strings
# - count duplicates; figure out way to prioritize crimes and delete. probably
#   ordered factors
# - run/load neighborhood/policebeat data
#       - count erroneous policebeats (excluding 77 or 99 which are unknown)
#       - mark police station crimes as unknown location
# - set up staffing levels etc for EDA


# reference copy for data that comes through the pipeline, delete later
crimeDT <- read_rds("Data/crime_240103.rds")



# Data Cleaning -----------------------------------------------------------

# TODO: convert Datetime to datetime

# Standardize column names
crimeDT <- crimeDT |> 
    rename_with(str_to_title,
                .cols = everything())

# Arrange crimes in chronological order
crimeDT <- crimeDT[order(Datetime)]

# Note: need to set up conditions so that too_few = "debug" if there are too few,
# but if everything is fine too_few doesn't add the 4 extra variables. So some 
# if/else for whether too_few exists (though this is expensive)

# Split Location column into X & Y coordinate columns
crimeDT <-
    crimeDT |> 
    filter(!Location == "") |> # Note: needed to add this to newest dataset bc
    # 4 entries (which were not new) were blank. How did they get converted with
    # the old dataset? TODO: check
    separate_wider_regex(
        Location,
        patterns = c(
            "POINT \\(",
            XCoord = "-[0-9]*.[0-9]*",
            " ",
            YCoord = "[0-9]*.[0-9]*",
            "\\)")) |> 
    # change back to data.table (separate_wider_regex() converted to tbl_df)
    as.data.table() |> 
    
    # change coordinates columns data types to double
    mutate(
        XCoord = parse_double(XCoord),
        YCoord = parse_double(YCoord)) |>
    # remove City & State columns
    select(-c(City, State))


# Standardize/rectify Crimetype categories --------------------------------

# Create CrimetypeOrig copy column
crimeDT <- crimeDT[, CrimetypeOrig := Crimetype] |> 
    relocate(CrimetypeOrig, .after = Crimetype)


# We see that last ~4 months of the dataset contain many missing Crimetype
# entries. Fortunately, the Description entries are not and can be used to fill 
# in Crimetype.

# find proportion of missing crimetype entries by year; 2023 and 2024 highest
crimeDT[Crimetype == "", .N, by = year(Datetime)][order(year)] |> 
    left_join(crimeDT[Crimetype != "", .N, by = year(Datetime)][order(year)],
              by = join_by(year)) |> 
    mutate(Proportion = N.x / (N.x + N.y)) |> print()
# which months in 2023 have highest empty entries
# last 4 months are bad; last 2 months especially bad
crimeDT[year(Datetime) == 2023 &
            Crimetype == "", .N, by = month(Datetime)][order(month)] |> 
    print()

# graph proportion of missing entries in 2023
crimeDT[year(Datetime) == 2023 &
            Crimetype == "", .N, by = month(Datetime)][order(month)] |> 
    left_join(crimeDT[year(Datetime) == 2023 &
                          Crimetype != "", 
                      .N, 
                      by = month(Datetime)][order(month)],
              by = join_by(month)) |> 
    mutate(Proportion = N.x / (N.x + N.y)) |> 
    ggplot(aes(x = month, y = Proportion)) + geom_line()
# TODO: add month labels


# Standardize Crimetype using Description to prepare for cleaning-----------

# Check for NA and placeholder Crimetype entries
crimeDT[is.na(Crimetype), ]
crimeDT[Crimetype == "OTHER", .N]
crimeDT[Crimetype == "PRE-LRMS/MISSING IN SYSTEM CONVERSION", .N]
crimeDT[Crimetype == "INCIDENT TYPE", .N]

# Populate NA or placeholder Crimetype entries with Description entries
crimeDT <- crimeDT |> mutate(
    Crimetype = if_else(
        (is.na(Crimetype) |
             Crimetype == "OTHER" |
            Crimetype == "PRE-LRMS/MISSING IN SYSTEM CONVERSION" |
             Crimetype == "INCIDENT TYPE"),
        Description,
        Crimetype))

# Remove entries lacking Description or Crimetype data
crimeDT <- crimeDT |> filter(!is.na(Description))


# Create vector containing only unique Crimetype entries, to help identify 
# & narrow down erroneous entries
distcrimes <- crimeDT[, .N, by = Crimetype][order(-N)]
distcrimes |> View()


# Problem: most listed homicides are actually unexplained deaths.
# These will be recategorized as unexplained deaths.
crimeDT[Crimetype == "HOMICIDE", ] |> 
    count(Description == "SC UNEXPLAINED DEATH") |> 
    print()

# Clean strings by consolidating Crimetypes with variations in wording/typos
# into main crime categories, then converting to factors
source("1.1_Clean_Strings.R")



# Identify/consolidate crimes with multiple entries -----------------------

# Check number of duplicates
crimeDT |> filter(duplicated(Casenumber)) |> View()
### There are about 100,000 duplicates; impossible manually. Solution:
### take same regex prioritization approach by inputting consequential
### Crimetypes and filtering the Description with those. TODO: finishing touches

# TODO: list all duplicates
crimeDT |> filter(Casenumber == "99-061943") |> View()


# Remove duplicate case numbers
crime <- crime |> distinct(CaseNumber, .keep_all = TRUE)





# Geographic verification -------------------------------------------------

source("01_Clean_Geography.R")

# NEW: moved Standardize/rectify police beats to geoprocessing file


# Verify data quality by observing beat observations with actual numbers
crimeDT |> distinct(Policebeat) |> count() # actual number of beats is 57
# View errors
crimeDT |> distinct(Policebeat) |> print(n = Inf)

# Compare OPD dataset's PoliceBeat with coordinate-verified beats
# ~8% of police beats are incorrectly listed
crimeDT |> count(Policebeat == Beat) |> 
    mutate(Proportion = n / sum(n)) |> print()





# EDA / Regressions -------------------------------------------------------------
source("EDA.R")
source("Regressions.R")



# Spatial Statistics ------------------------------------------------------
source("05_Spatial_Stats.R")
source("GI_Neighbs.R")




