library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(data.table)
library(tmap)
library(sf)
library(terra)
library(spdep)
library(ggplot2)

source("Crime_Processing.R")



cdt <- read_rds("Data/cDTclean.rds")
demone <- read_rds("Data/demone.rds")
demopb <- read_rds("Data/demopb.rds")
crimebyyear <- read_rds("Data/crimebyyear.rds")
pbtot <- read_rds("Data/pbtotcrime.rds")
netot <- read_rds("Data/netotcrime.rds")
censtot <- read_rds("Data/censtotcrime.rds")


# EDA for clean data ------------------------------------------------------

# Narrow down major crimes
maj <- c("homicide", "robbery", "auto theft", "carjacking", 
         "theft", "burglary")

# Narrow down violent crimes
violent <- c("homicide", "assault", "robbery", "weapons",
             "carjacking")

# Narrow down property crimes (note: also counting robbery & carjacking)
property <- c("theft", "auto theft", "burglary", "robbery", 
              "fraud", "carjacking")

# Define months for labels
months <- c("January", "February", "March", "April",
            "May", "June", "July", "August", "September",
            "October", "November", "December")



# Visualizations ----------------------------------------------------------

# Citywide

# Per capita staffing year by year
staffing |> 
    ggplot(aes(x = Year,
               y = Staff_PerCap)) +
    geom_line() +
    lims(y = c(0,2.5))
# zoom in
staffing |>
    ggplot(aes(x = Year,
               y = Staff_PerCap)) +
    geom_line()


# Problem: The current month skews averages for year and month
# downward. 
# Solution: Drop current month from dataset, and compute averages
# across year-months instead of across years. This way the year 
# does not have to be complete to compute the average.
cdt <- cdt[Datetime < floor_date(range(cdt$Datetime)[2], "month")]

# Total crime by year
cdt[, 
    .(Totalcrime = .N),
    by = (Yearmonth = format(Datetime, "%Y%m"))][order(Yearmonth)] |>
    mutate(Year = substr(Yearmonth, 1, 4)) |> 
    group_by(Year) |>
    summarise(Av_Crime = mean(Totalcrime)) |>
    ggplot(aes(x = as.numeric(Year),
               y = Av_Crime)) +
    geom_line() +
    labs(
        title = "Total Reported Average Crime Per Year",
        x = "Year",
        y = "Average Crime")

# Total crime by month
# Crime significantly drops in February
cdt[, 
    .(Totalcrime = .N),
    by = month(Datetime)][order(month)] |>
    ggplot(aes(x = month,
               y = Totalcrime)) +
    geom_line() +
    labs(
        title = "Total Reported Average Crime Per Month",
        x = "Month",
        y = "Average Crime") +
    lims(x = months) +
    theme(axis.text.x = 
              element_text(angle = 45, hjust = 1)) 


# TODO: add temperature/precipitation data

# Yearly trends -----------------------------------------------------------


# Function that graphs yearly trend
yearly_trend <- function(crime) {
    cdt[Year > 2004 & 
            Year < 2024 &
            Crimetype == crime, .N, by = Year ] |> 
        ggplot(aes(
            x = Year,
            y = N)) + 
        geom_line() +
        labs(
            title = paste0("Yearly trend of ", crime),
            y = "Rate")
}

# Function that graphs normalized yearly trend
yearly_trend_norm <- function(crime) {
    crime <- paste0(crime, "_norm")
    
    netot_normDT |>
        group_by(Year) |>
        summarise(V1 = mean(.data[[crime]], na.rm = T)) |> 
        ggplot(aes(
            x = Year,
            y = V1)) +
        geom_line() +
        labs(
            title = paste0("Yearly trend of ", crime, "\n(normalized)"),
            y = "Normalized rate per\n1000 residents")
}
# TODO: fix x labels

# Homicide
yearly_trend("homicide")
yearly_trend_norm("homicide")

# Assault
yearly_trend("assault")
yearly_trend_norm("assault")

# Weapons
yearly_trend("weapons")
yearly_trend_norm("weapons")

# Auto theft
yearly_trend("auto theft")
yearly_trend_norm("auto theft")

# Theft
yearly_trend("theft")
yearly_trend_norm("theft")

# Burglary
yearly_trend("burglary")
yearly_trend_norm("burglary")

# Arson
yearly_trend("arson")
yearly_trend_norm("robbery")

# Unexplained death
# TODO
yearly_trend("unexdeath")
yearly_trend_norm("unexdeath")

# Sex offense
yearly_trend("sex offense")
yearly_trend_norm("sex offense")

# Vandalism
yearly_trend("vandalism")
yearly_trend_norm("vandalism")

# Traffic
yearly_trend("traffic")
yearly_trend_norm("traffic")

# DUI
yearly_trend("dui")
yearly_trend_norm("dui")

# Fraud
yearly_trend("fraud")
yearly_trend_norm("fraud")

# Carjacking
# TODO: replace once names are re-cleaned
cdt[Year > 2004 & 
        str_detect(Description, "CARJ"), ] |> 
    group_by(Year) |> 
    summarise(N = n()) |> 
    ggplot(aes(
        x = Year,
        y = N
    )) + geom_line()





# TODO

# Crime rate (per 100,000 inhabitants) year by year
# Note: population figures only available from 2009-2022
cdt[, 
    .(Totalcrime = .N),
    by = (Year)][order(Year)] |>
    inner_join(yearlypop, 
              by = join_by(Year)) |>
    mutate(Rate100K = Totalcrime / TotalPop * 100000) |>  
    ggplot(aes(x = Year,
               y = Rate100K)) +
    geom_line() +
    labs(
        title = "Total Reported Crime Per Year",
        x = "Year",
        y = " Crime Total")


# Per capita violent crime rate year by year
cdt[Crimetype %in% violent, 
    .(Violentcrime = .N),
    by = (Year)][order(Year)] |>
    inner_join(yearlypop, 
               by = join_by(Year)) |>
    mutate(Rate100K = Violentcrime / TotalPop * 100000) |>  
    ggplot(aes(x = Year,
               y = Rate100K)) +
    geom_line() +
    labs(
        title = "Total Reported Violent Crime Rate Per Year",
        x = "Year",
        y = "Violent Crime Rate Per 100,000 Citizens")


# Per capita property crime year by year
cdt[Crimetype %in% property, 
    .(Totalcrime = .N),
    by = (Year)][order(Year)] |>
    inner_join(yearlypop, 
               by = join_by(Year)) |>
    mutate(Rate100K = Totalcrime / TotalPop * 100000) |>  
    ggplot(aes(x = Year,
               y = Rate100K)) +
    geom_line() +
    labs(
        title = "Total Reported Property Crime Rate Per Year",
        x = "Year",
        y = "Property Crime Rate Per 100,000 Citizens")


# Per capita homicide year by year
# Note: this will change after re-cleaning strings
cdt[str_detect(Crimetype, "homicide"), 
    .(Totalcrime = .N),
    by = (Year)][order(Year)] |>
    inner_join(yearlypop, 
               by = join_by(Year)) |>
    mutate(Rate100K = Totalcrime / TotalPop * 100000) |>  
    ggplot(aes(x = Year,
               y = Rate100K)) +
    geom_line() +
    labs(
        title = "Reported Property Crime Rate Per Year",
        x = "Year",
        y = "Homicide Rate Per 100,000 Citizens")


# Neighborhoods ranked by per capita total crime
total_PCrate <- 
    netot_norm |> 
    st_drop_geometry() |> 
    group_by(Neighborhood) |>
    summarize(Total_Norm = sum(pick(
        ends_with("norm")))) |> 
    arrange(desc(Total_Norm))
total_PCrate



# Neighborhoods with highest per capita violent crime
violent_PCrate <-
    netot_norm |> 
    st_drop_geometry() |> 
    group_by(Neighborhood) |>
    summarize(Violent_Norm = sum(
        pick(starts_with(violent) &
                 ends_with("norm")))) |> 
    arrange(desc(Violent_Norm))
    

# Neighborhoods with highest per capita property crime
property_PCrate <-
    netot_norm |> 
    st_drop_geometry() |> 
    group_by(Neighborhood) |>
    summarize(Property_Norm = sum(
        pick(starts_with(property) &
                 ends_with("norm")))) |> 
    arrange(desc(Property_Norm))



# Identify highest/lowest neighborhoods for regressions ---------------------

# identify top 10 highest neighborhoods for total crime per capita
top10PCtotal <- total_PCrate |> 
    distinct(Neighborhood) |> 
    head(10) |> 
    pull()

# identify lowest 10 highest neighborhoods for total crime per capita
lowest10PCtotal <- total_PCrate |> 
    distinct(Neighborhood) |> 
    tail(10) |> 
    pull()


# identify top 10 neighborhoods for violent crime per capita
top10PCviolent <-
    violent_PCrate |> 
    distinct(Neighborhood) |> 
    head(10) |> 
    pull()
# identify lowest 10 neighborhoods for violent crime per capita
lowest10PCviolent <-
    violent_PCrate |> 
    distinct(Neighborhood) |> 
    tail(10) |> 
    pull()

# identify top 10 neighborhoods for property crime per capita
top10PCproperty <-
    property_PCrate |> 
    distinct(Neighborhood) |> 
    head(10) |> 
    pull()
# identify lowest 10 neighborhoods for property crime per capita
lowest10PCproperty <-
    property_PCrate |> 
    distinct(Neighborhood) |> 
    tail(10) |> 
    pull()




# Neighborhood plots ------------------------------------------------------

# TODO: create GIFs
# TODO: function factory

# Function to plot rates of total crime for given year
# NOTE: can only use for years 2009-2022, since demographics
# are not yet available in other years
plot_total_rates_neighb <- function(year) {
    netot_norm |> 
        # drop geometry for faster computation
        st_drop_geometry() |> 
        filter(Year == year) |> 
        group_by(Neighborhood, Year) |>
        summarize(Total_Norm = sum(
            pick(ends_with("norm")))) |> 
        arrange(Neighborhood) |> 
        left_join(neighbs) |> 
        st_as_sf(crs = 4269) |> 
        tm_shape() +
        tm_fill("Total_Norm") +
        tm_borders()
}
plot_total_rates_neighb(2019)
# TODO: insert scale for legend that goes up to 1300 (highest
# is 1301)

# Function to plot rates of violent crime for given year
# NOTE: can only use for years 2009-2022, since demographics
# are not yet available in other years
plot_violent_rates_neighb <- function(year) {
    netot_norm |> 
        # drop geometry for faster computation
        st_drop_geometry() |> 
        filter(Year == year) |> 
        group_by(Neighborhood, Year) |>
        summarize(Violent_Norm = sum(
            pick(starts_with(violent) &
                     ends_with("norm")))) |> 
        arrange(Neighborhood) |> 
        left_join(neighbs) |> 
        st_as_sf(crs = 4269) |> 
        tm_shape() +
        tm_fill("Violent_Norm") +
        tm_borders()
}
plot_violent_rates_neighb(2022)
# TODO: insert scale for legend that goes up to 300

# Function to plot rates of property crime for given year
# NOTE: can only use for years 2009-2022, since demographics
# are not yet available in other years
plot_property_rates_neighb <- function(year) {
    netot_norm |> 
        # drop geometry for faster computation
        st_drop_geometry() |> 
        filter(Year == year) |> 
        group_by(Neighborhood, Year) |>
        summarize(Property_Norm = sum(
            pick(starts_with(property) &
                     ends_with("norm")))) |> 
        arrange(Neighborhood) |> 
        left_join(neighbs) |> 
        st_as_sf(crs = 4269) |> 
        tm_shape() +
        tm_fill("Property_Norm") +
        tm_borders()
}
plot_property_rates_neighb(2019)
# TODO: insert scale for legend that goes up to 800



# Police beat plots -------------------------------------------------------

# TODO: add police beats counterpart to section above

