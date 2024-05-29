library(dplyr)
library(tidyr)
library(data.table)
library(sf)

library(readr)
crimeDT <- read_rds("Data/crime240502.rds")


# Aggregate crimes citywide to prepare for exploratory data analysis------------
crimeDT$Year <- year(crimeDT$Datetime)
crimeDT |> write_rds("Data/crime240502.rds")

# create crimes list from 2005-2024, for which data is more reliable
years <- c(2005:2024)
crimeDT <- crimeDT[crimeDT$Year > 2004, ]
crimes <- crimeDT |> 
    filter(Year %in% years) |> 
    distinct(Crimetype)
# convert crimes from factor to character
crimes[] <- lapply(crimes, as.character)
crimes <- crimes[[1]]

# create crime by year table with column for year and empty 
# column for each crime type
crimebyyear <- data.table(Year = c(2005:2024)) |> 
    cbind(crimes) |> 
    suppressWarnings()
# remove recycled crime values
crimebyyear <- crimebyyear |> mutate(
    crimes = case_when(
        duplicated(crimes) ~ NA,
        .default = crimes))
# pivot out crime rows into empty columns
crimebyyear <-
    crimebyyear |>
    mutate(a = NA) |> 
    pivot_wider(names_from = crimes,
                values_from = a) |>
    # drop NA column
    select(1:20)

# Function that tabulates crime totals by year for each crime.
# Note that data.table's join creates additional columns with
# prefix .i. that I remove in the for loop below. It also
# reverses the column order, which I also correct in the for loop.
fdt <- function(dt, index) {
    name <- crimes[index]
    
    dttemp <- 
        crimeDT[((Year > 2004) &
                     Year <= 2024) &
                    Crimetype %in% crimes[index],
                .N,
                by = .(Year)][order(Year)] |>
            rename(!!name := N)

    dttemp[dt, on = .(Year)]
    }

# TODO: perform join without creating additional .i. columns
# Note: used order (length(crimes)):1 so that it matches original
# order, just for neatness' sake.
for (i in (length(crimes)):1) {
    crimebyyear <- fdt(crimebyyear, i) |> 
    # kludge to remove added columns
    select(!contains("i."))}

crimebyyear |> write_rds("Data/crimebyyear240502.rds")



# Aggregate crimes by neighborhood/beat ----------------------------------------

# function that calculates count for given crimetype by year
aggregate_crime <- function(index, crimesfile, year, demo) {
    # subset crime file with crimetype argument
    crimename <- crimesfile[index] # access list element
    crime <- substitute(crimename) # get crime col name
    crimesheet <- cgeo[cgeo$Crimetype == crime, ] # subset
    
    # assign new column for aggregated crime total
    demo[{{crime}}] <- 
        # aggregate crime totals per neighborhood per year
        map(year,
            \(y, demo, crimesheet)
            demo |>
                group_by(Year) |>
                filter(Year == y) |>
                st_intersects(crimesheet |> filter(Year == y)) |>
                map_int(length), demo, crimesheet) |>
        unlist()
    demo
}



# Because it's based on census data, demone has a different year range
# defined here as y
y <- range(demone$Year)[1]:range(demone$Year)[2]

# add aggregates for each crime for neighborhoods
for (i in 1:length(crimes)) {
    demone <- aggregate_crime(i, crimes, years, demone)
}
# note: used years instead of y as arg for demone, prob mistakenly

# add aggregates for each crime for police beats
y <- range(demopb$Year)[1]:range(demopb$Year)[2]
for (i in 1:length(crimes)) {
    demopb <- aggregate_crime(i, crimes, y, demopb)
} |> View()

# add aggregates for each crime for census units
y <- range(demoacs$Year)[1]:range(demoacs$Year)[2]
for (i in 1:length(crimes)) {
    demoacs <- aggregate_crime(i, crimes, y, demoacs)
}


demone |> write_rds("netotcrime.rds")
demopb |> write_rds("pbtotcrime.rds")
demoacs |> write_rds("censtotcrime.rds")











# Percent change from year to year
# function that calculates percent change from last year
pct_change <- function(crimepanel) {
    crimepanel |>
        # group by Neighborhood, Beat, or GEOID, which are in 2nd col
        group_by(crimepanel[2]) |>
        # compute percentage change from last year for all crimes
        mutate(
            pct_ch =
                across(theft:Other,
                       \(x) 100 * (x - lag(x)) /
                           (lag(x) + .001)) |> # avoid divide by 0
                round(digits = 2)
        )
}
pct_change(st) |> View()

# TODO: consider making a new dataframe with percent change and 
# averaged demographics from the two parent years



# TODO: see whether necessary to make this 100,000 for consistency with
# federal standards

# Normalized rates per 1,000 people

# function that normalizes crime rates per 1,000 residents
# according to specified zone
# NOTE: some crime names have spaces in them
normalize_rate <- function(demog) {
    demog |>
        mutate(
            Rate =
                across(theft:Other,
                       \(x) 1000 * (x / (Pop + 1)) |>
                           round(digits = 5)) |> 
                rename_with(\(x) paste0(x, "_norm"))
        ) |> 
        relocate(geometry, .after = Rate) |> 
        unnest(Rate)
}
censtot <- censtot |> rename(Pop = pop)

netot_norm <- normalize_rate(netot)
censtot_norm <- normalize_rate(censtot)
pbtot_norm <- normalize_rate(pbtot)

# Per capita
percapita_rate <- function(demog) {
    demog |>
        mutate(
            PerCap_Rate =
                across(theft:Other,
                       \(x) 1000 * (x / sum(Pop)) |>
                           round(digits = 5)) |> 
                rename_with(\(x) paste0(x, "_Percap"))
        ) |> 
        relocate(geometry, .after = PerCap_Rate) |> 
        unnest(PerCap_Rate)
}
netot_pc <- percapita_rate(netot) |> st_drop_geometry() |> as.data.table()


# Standardize rates (z-score)

# function that creates standardized rates of specified crime
# according to specified zone
standardize_rate <- function(demog, crime) {
    # create name
    nameZ <- paste0(as.character(crime), "_Z")
    # drop geometry; otherwise doesn't work
    d <- demog |> st_drop_geometry()  
    # calculate standardized crime rate
    
    # calculate mean and standard deviation
    m <- d |> pull(crime) |> mean(na.rm = T)
    s <- d |> pull(crime) |> sd(na.rm = T)
    
    # create standardized crime column
    d[{{nameZ}}] <- 
        d[{{crime}}] - (m / s)
    # return dataframe
    d
}
standardize_rate(demone, "arson") |> View()

# plot
breaks = c(-10, -1, 0, 1, 10)
plot(demone["robberyZ"], breaks=breaks, 
     pal=colorRampPalette(c("navy", "gray", "red")))


# check distribution
hist(demone$robberyZ)



# Add per capita police officer count for 1000 citizens for whole city

# compute city population
population <- censtot |> 
    st_drop_geometry() |> 
    group_by(Year) |> 
    summarise(Pop = sum(pop))

# Add per capita staff variable
censtot <-
    censtot |> 
    left_join(
        # create new staff per capita variable Staff_PerCap
        inner_join(population, staffing,
                   by = c("Year", "Pop")) |>
            mutate(Staff_PerCap = 1000 * (Staff / Pop)) |> 
            select(Year, Staff_PerCap),
        # join new variable
        by = join_by(Year))


# Staffing levels ---------------------------------------------------------

staff <- c(675, 743, 732, 775, 756, 704, 683, 699, 736, 830, 780, 656, 642,
           613, 626, 695, 721, 744, 747, 749, 730, 723, 790)
# note: check 2022
years <- c(2000:2022)
staffing <- tibble(Staff = staff, Year = years)
# source:
# https://oaklandca.s3.us-west-1.amazonaws.com/oakca1/groups/police/documents/webcontent/oak053167.pdf
# https://cao-94612.s3.us-west-2.amazonaws.com/documents/InfoReport_OPDStaffing_10.18.19.pdf
# https://cao-94612.s3.us-west-2.amazonaws.com/documents/OPD-Q2-Staffing-Memo-9.23.22.pdf


staffing <- inner_join(population, staffing,
                       by = "Year") |> 
    mutate(Staff_PerCap = 1000 * (Staff / Pop))

# convert to data.table for faster computations
censtot_normDT <- censtot_norm |> st_drop_geometry() |> as.data.table()
netot_normDT <- netot_norm |> st_drop_geometry() |> as.data.table()
