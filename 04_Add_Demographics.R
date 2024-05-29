library(sf)
library(readr)
library(dplyr)
library(purrr)


# Setup/file preparation --------------------------------------------------


# Import demographic files
source("Census.R")
source("01_Clean_Geography.R")

popacs <- read_rds("Data/popacs.rds")
incageacs <- read_rds("Data/incageacs.rds")
raceacs <- read_rds("Data/raceacs.rds")
demoacs <- read_rds("Data/demoacs.rds")


# Create demographic dataframes -------------------------------------------

# census tract/block group demographics. As granular as possible
demoacs <-
    popacs |> 
    # drop geometry to do nonspatial left join
    st_drop_geometry() |> 
    left_join(incageacs |> select(Year, GEOID, medage, medinc),
              join_by(Year, GEOID)) |>
    left_join(raceacs |> select(Year, GEOID, White:Hispanic, geometry),
              join_by(Year, GEOID, geometry)) |> 
    relocate(geometry, .after = Hispanic)
# clean up some columns
demoacs <-
    demoacs |> select(c(-c(moe, variable), Pop = estimate))
# add back geometry
st_geometry(demoacs) <- demoacs$geometry

demoacs |> write_rds("Data/demoacs.rds")

# TODO: see if this is still necessary or if possible to use vector of
# years from the demography dfs
years <- c(2010:2022)

# Create group of functions that take police beat or neighborhood file and 
# demography file as arguments and calculate population using weighted sum of 
# the ACS block group/tract area that falls within the police beat. Since these 
# are the most granular unit of geometry in which population estimates are 
# available, this function assumes that population is evenly distributed across 
# the block group's area, though this is not always the case.


# Update 4/3/24: Refactored functions by breaking into smaller component
# functions and calling inside functions. Next step is to create function
# factories.
# Also made a small performance improvement by dropping geometries
# before computing summary statistics.

# Break into smaller functions

# Create zone
create_zone <- function(neigh, shapefile, demog) {
    # define area of interest
    zone <- shapefile[shapefile[[1]] == neigh, drop = F]
   
    ## Superassign next 2 vars to parent environment, for use in main function
    # clip block groups to fit boundaries of zone
    zonearea <<- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <<- zonearea$GEOID
}
create_zone("Chinatown", neighbs, popacs)

# Compute area weights
compute_area <- function(demog, zonegeos, GEOID) {
    computed_area <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and 
        #   area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry)))
    computed_area
}

area_weights_pop <- function(computed_area) {
    computed_area |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal))
}

weighted_sum_pop <- function(comparison_area) {
    # return weighted sum: population estimate taking into account
    # area of GEOID
    sum(comparison_area$estimate * comparison_area$Proportion)
}

# calculate neighborhood population
narea_weights_pop <- function(neigh, shapefile, demog) {
    
    create_zone(neigh, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_pop()
    
    weighted_sum_pop(comparison)
}
# test
narea_weights_pop(
    "Chinatown", neighbs, popacs |> 
        filter(Year == 2020))

# calculate police beat population
pbarea_weights_pop <- function(beat, shapefile, demog) {
    
    create_zone(beat, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_pop()
    
    weighted_sum_pop(comparison)
}
# test
pbarea_weights_pop(
    "02X", policebeats, popacs |> 
        filter(Year == 2020))


# Calculate race
area_weights_race <- function(computed_area) {
    computed_area |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal),
               weights = Proportion / sum(Proportion))
}
weighted_sum_race <- function(comparison_area) {
    comparison_area |> 
        st_drop_geometry() |> 
        # to retain column for joining function output
        # comparison[[15]] is beat or neighorhood
        group_by(AreaUnit = comparison_area[[15]]) |> 
        
        # calculate proportions based on area size
        summarize(
            across(White:Hispanic,
                   \(x) with(comparison_area, 
                             sum(x * weights, na.rm = TRUE)))) |> 
        # calculate sum
        mutate(Sum = sum(White, Black, Native,
                         Asian, HIPI, Hispanic,
                         na.rm = T)) |> 
        # calculate percentages for area
        summarise(AreaUnit,
            across(White:Hispanic,
                   \(x) x/Sum * 100))
}


# Calculate neighborhood race
narea_weights_race <- function(neigh, shapefile, demog) {
    
    create_zone(neigh, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_race()
    
    # return weighted mean of race
    weighted_sum_race(comparison)
}
# test
narea_weights_race(
    "Chinatown", neighbs, raceacs |> 
        filter(Year == 2020))



# Calculate police beat race
pbarea_weights_race <- function(beat, shapefile, demog) {
    
    create_zone(beat, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_race()
    
    # return weighted mean of race
    weighted_sum_race(comparison)
}
# test
pbarea_weights_race(
    "01X", policebeats, raceacs |> 
        filter(Year == 2020))
pbarea_weights_race(
    "01X", policebeats, raceacs |> 
        filter(Year == 2020)) |> sum()


# Calculate neighborhood income
area_weights_income <- function(computed_area) {
    computed_area |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        mutate(weights = Proportion / sum(Proportion))
}
weighted_sum_income <- function(comparison_area) {
    # return weighted sum: population estimate taking into account
    #   area of GEOID
    # manual weighted means b/c weighted.mean() doesn't accept na.rm arg
    with(
        # prevent NAs from enlarging denominator
        comparison_area |> filter(!is.na(medinc)), 
        sum(medinc * weights, na.rm = TRUE) / 
            sum(weights, na.rm = TRUE))
}

narea_weights_medinc <- function(neigh, shapefile, demog) {

    create_zone(neigh, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_income()
    
    weighted_sum_income(comparison)
}
# test
narea_weights_medinc(
    "Chinatown", neighbs, incageacs |> 
        filter(Year == 2020))

# TODO: may have to substitute Medinc and Medage for medinc and medage once
# there is a new demographic file

# calculate police beat income
# Note: ages are not median ages found in ACS; they are averages 
pbarea_weights_medinc <- function(beat,  shapefile, demog) {
    
    create_zone(beat, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_income()
    
    weighted_sum_income(comparison)
}
# test
pbarea_weights_medinc(
    "01X", policebeats, incageacs |> 
        filter(Year == 2020))



# Calculate neighborhood age

area_weights_age <- function(computed_area) {
    computed_area |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        mutate(weights = Proportion / sum(Proportion))
}
weighted_sum_age <- function(comparison_area) {
    # return weighted sum: population estimate taking into account
    #   area of GEOID
    # manual weighted means b/c weighted.mean() doesn't accept na.rm arg
    with(
        # prevent NAs from enlarging denominator
        comparison_area |> filter(!is.na(medage)), 
        sum(medage * weights, na.rm = TRUE) / 
            sum(weights, na.rm = TRUE))
}

# Note: ages are not median ages found in ACS; they are averages 
narea_weights_medage <- function(neigh, shapefile, demog) {
    
    create_zone(neigh, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_age()
    
    weighted_sum_age(comparison)
}
# test
narea_weights_medage(
    "Chinatown", neighbs, incageacs |> 
        filter(Year == 2020))


# calculate police beat age
# Note: ages are not median ages found in ACS; they are averages 
pbarea_weights_medage <- function(beat, shapefile, demog) {
    
    create_zone(beat, shapefile, demog)
    
    comparison <- 
        compute_area(demog, zonegeos, GEOID) |>
        area_weights_age()
    
    weighted_sum_age(comparison)
}
# test
pbarea_weights_medage(
    "01X", policebeats, incageacs |> 
        filter(Year == 2020))








# TODO: Function factory --------------------------------------------------


area_weights_pop <- function(areaunit, shapefile) {
    function(areaunit, shapefile, demog) {
        
        create_zone(areaunit, shapefile, demog)
        
        comparison <- 
            compute_area(demog, zonegeos, GEOID) |>
            area_weights_pop()
        
        weighted_sum_pop(comparison)
    }
}




# Create dataframes -------------------------------------------------------

# map/wrap functions across each year
pbadd_stats <- function(year) {
    funcs = c(pbarea_weights_pop,
              pbarea_weights_medage,
              pbarea_weights_medinc,
              pbarea_weights_race)
    
    policebeats |>
        mutate(Pop = map_dbl(policebeats$Beat,
                             funcs[[1]],
                             policebeats,
                             popacs |> filter(Year == year)),
               Age = map_dbl(policebeats$Beat,
                             funcs[[2]],
                             policebeats,
                             incageacs |> filter(Year == year)),
               Income = map_dbl(policebeats$Beat,
                                funcs[[3]],
                                policebeats,
                                incageacs |> filter(Year == year)),
        ) |>
        left_join(map_df(policebeats$Beat,
                         funcs[[4]],
                         policebeats,
                         raceacs |> filter(Year == year)),
                  join_by(Beat == AreaUnit)) |>
        mutate(Year = as.integer(year),
               .before = Beat)
}

nadd_stats <- function(year) {
    funcs = c(narea_weights_pop,
              narea_weights_medage,
              narea_weights_medinc,
              narea_weights_race)
    
    neighbs |>
        mutate(Pop = map_dbl(neighbs$Neighborhood,
                             funcs[[1]],
                             neighbs,
                             popacs |> filter(Year == year)),
               Age = map_dbl(neighbs$Neighborhood,
                             funcs[[2]],
                             neighbs,
                             incageacs |> filter(Year == year)),
               Income = map_dbl(neighbs$Neighborhood,
                                funcs[[3]],
                                neighbs,
                                incageacs |> filter(Year == year)),
        ) |>
        left_join(map_df(neighbs$Neighborhood,
                         funcs[[4]],
                         neighbs,
                         raceacs |> filter(Year == year)),
                  join_by(Neighborhood == AreaUnit)) |>
        mutate(Year = as.integer(year),
               .before = Neighborhood)
}



# Create demographics file for policebeats and neighborhoods
demopolbeat <- map_df(years, pbadd_stats) # 11:30
demoneighb <- map_df(years, nadd_stats) # 17:30
# Reduced compute time, but can probably reduce it further.
# TODO: see if clustering reduces compute time
# TODO: explore whether sfheaders can do st_area() type alg or,
#   alternatively, whether loading shps with it goes faster

# Reorder
demopolbeat <- demopolbeat |> relocate(geometry, .after = Hispanic)
demoneighb <- demoneighb |> relocate(geometry, .after = Hispanic)



write_rds(demopolbeat, "Data/demopolbeat0529.rds")
write_rds(demoneighb, "Data/demoneighb0529.rds")


