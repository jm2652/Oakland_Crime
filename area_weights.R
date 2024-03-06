library(sf)
library(readr)
library(dplyr)


# Setup/file preparation --------------------------------------------------

# Load neighborhood and police beat shapefiles
setwd("../../Oakland_Mapping/Police_Beats")
policebeats <- read_sf("policebeats.shp")
# worked on this first but it was the original file that I hadn't corrected.
# But it doesn't throw vertex errors so I'll use it
setwd("../Neighborhoods/neighborhoods_20231030")
neighbs <- read_sf("geo_export_8681fdd8-61be-4add-80cb-f7bc61dae57f.shp")
# TODO: use this once the .shp is corrected
# setwd("Oakland_Mapping/Neighborhoods/Neighborhoods_edit")
# neighbs <- read_sf("neighborhoods_edit.shp")
setwd("../../../Oakland_Projects/Oakland_Crime")


# process mapfiles
neighbs <- neighbs |> 
    select(Neighborhood = neighbhd,
           geometry) |> 
    st_transform(crs = 4269) |> 
    arrange(Neighborhood)

# TODO: change these in shapefile
neighbs$Neighborhood[22] <- "Coliseum Airport"
neighbs$Neighborhood[31] <- "East 14th Street Business 2"

policebeats <- policebeats |> 
    select(Beat = name,
           # District = pol_dist, # of course it's wrong. TODO:
           # create df with correct districts
           geometry) |> 
    st_transform(crs = 4269) |> 
    arrange(Beat)


# Import demographic files
popacs <- read_rds("popacs.rds")
incageacs <- read_rds("incageacs.rds")
raceacs <- read_rds("raceacs.rds")


# Create demographic dataframes -------------------------------------------

# TODO: see if this is still necessary or if possible to use vector of
# years from the demography dfs
years <- c(2010:2022)

# Create group of functions that take police beat or neighborhood file and 
# demography file as arguments and calculate population using weighted sum of 
# the ACS block group/tract area that falls within the police beat. Since these 
# are the most granular unit of geometry in which population estimates are 
# available, this function assumes that population is evenly distributed across 
# the block group's area, though this is not always the case.


# calculate neighborhood population
narea_weights_pop <- function(neigh, demog) {
    # define which neighborhood / zone
    zone <- neighbs[neighbs$Neighborhood == neigh, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        # area weights
        mutate(pops = estimate * Proportion)
    # return weighted sum: population estimate taking into account
    # area of GEOID
    sum(comparison$estimate * comparison$Proportion)
}
# test
narea_weights_pop("Chinatown", pop20acs)

# calculate police beat population
pbarea_weights_pop <- function(pb, demog) {
    # define which police beat / zone
    zone <- policebeats[policebeats$Beat == pb, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        # area weights
        mutate(pops = estimate * Proportion)
    # return weighted sum: population estimate taking into account
    # area of GEOID
    sum(comparison$estimate * comparison$Proportion)
}
# test
pbarea_weights_pop("12Y", pop20acs)


# Race --------------------------------------------------------------------

# calculate neighborhood race
narea_weights_race <- function(neigh, demog) {
    # define which neighborhood / zone
    zone <- neighbs[neighbs$Neighborhood == neigh, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal),
               weights = Proportion / sum(Proportion))
    # return weighted mean of race
    comparison |> 
        group_by(Neighborhood) |> # to retain column for joining function output
        summarize(
            across(White:Hispanic,
            \(x) with(comparison, sum(x * weights, na.rm = TRUE) /
                              sum(weights, na.rm = TRUE)))) |> 
        st_drop_geometry()
    # This will be harder to do. TODO: defend against NAs in means across
}



# calculate police beat race
pbarea_weights_race <- function(pb, demog) {
    # define which neighborhood / zone
    zone <- policebeats[policebeats$Beat == pb, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal),
               weights = Proportion / sum(Proportion))
    # return weighted mean of race
    comparison |> 
        group_by(Beat) |> # to retain column for joining function output
        summarize(
            across(White:Hispanic,
                   \(x) with(comparison, sum(x * weights, na.rm = TRUE) /
                                 sum(weights, na.rm = TRUE)))) |> 
        st_drop_geometry()
    # This will be harder to do. TODO: defend against NAs in means across
}
# test
pbarea_weights_race("12Y", raceacs |> filter(Year == 2015))



# calculate neighborhood income
narea_weights_medinc <- function(neigh, demog) {
    # define which neighborhood / zone
    zone <- neighbs[neighbs$Neighborhood == neigh, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        mutate(weights = Proportion / sum(Proportion))
    # return weighted sum: population estimate taking into account
    # area of GEOID
    # weighted.mean(comparison$estimate * comparison$Proportion)
    
    # doesn't do na.rm; so use below instead
    # weighted.mean(comparison$medincE, as.numeric(comparison$Proportion))
    
    # manual weighted means
    with(
        # prevent NAs from enlarging denominator
        comparison |> filter(!is.na(medinc)), 
         sum(medinc * weights, na.rm = TRUE) / 
             sum(weights, na.rm = TRUE))
}
# test
narea_weights_medinc("Allendale")


# calculate police beat income
# Note: ages are not median ages found in ACS; they are averages 
pbarea_weights_medinc <- function(pb, demog) {
    # define which neighborhood / zone
    zone <- policebeats[policebeats$Beat == pb, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        mutate(weights = Proportion / sum(Proportion))
    # return weighted sum: population estimate taking into account
    # area of GEOID
    # weighted.mean(comparison$estimate * comparison$Proportion)
    
    # doesn't do na.rm; so use below instead
    # weighted.mean(comparison$medincE, as.numeric(comparison$Proportion))
    
    # manual weighted means
    with(
        # prevent NAs from enlarging denominator
        comparison |> filter(!is.na(medinc)), 
        sum(medinc * weights, na.rm = TRUE) / 
            sum(weights, na.rm = TRUE))
}
# test
pbarea_weights_medinc("01X")

# calculate neighborhood age
# Note: ages are not median ages found in ACS; they are averages 
narea_weights_medage <- function(neigh, demog) {
    # define which neighborhood / zone
    zone <- neighbs[neighbs$Neighborhood == neigh, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        mutate(weights = Proportion / sum(Proportion))
    # return weighted sum: population estimate taking into account
    # area of GEOID
    # weighted.mean(comparison$estimate * comparison$Proportion)
    
    # doesn't do na.rm; so use below instead
    # weighted.mean(comparison$medageE, as.numeric(comparison$Proportion),
    #               na.rm = TRUE)
    
    # manual weighted means
    with(
        # prevent NAs from enlarging denominator
        comparison |> filter(!is.na(medage)), 
        sum(medage * weights, na.rm = TRUE) / 
            sum(weights, na.rm = TRUE))
}
# test
narea_weights_medage("Sequoyah")


# calculate police beat age
# Note: ages are not median ages found in ACS; they are averages 
pbarea_weights_medage <- function(pb, demog) {
    # define which neighborhood / zone
    zone <- policebeats[policebeats$Beat == pb, drop = F]
    # clip block groups to fit boundaries of zone
    zonearea <- st_intersection(demog, zone) |> 
        suppressWarnings()
    # list GEOIDs that fall within zone
    zonegeos <- zonearea$GEOID
    
    # compare total area of GEOIDs with clipped area
    # filter only GEOIDs that intersect with the zone
    comparison <- 
        demog |> 
        filter(GEOID %in% zonegeos) |> 
        arrange(GEOID) |> # ensure that rows are aligned for cbind
        # get total area of block groups and area of clipped block groups by GEOID
        group_by(GEOID) |> 
        # total area of each GEOID 
        summarise(AreaTotal = st_area(geometry)) |> 
        # join with zone-clipped area of each GEOID
        cbind(
            zonearea |> 
                arrange(GEOID) |>
                group_by(GEOID) |> 
                mutate(AreaWithin = st_area(geometry))
        ) |> 
        # find proportion, stripping m^2 unit
        mutate(Proportion = as.numeric(AreaWithin / AreaTotal)) |>
        mutate(weights = Proportion / sum(Proportion))
    # return weighted sum: population estimate taking into account
    # area of GEOID
    # weighted.mean(comparison$estimate * comparison$Proportion)
    
    # doesn't do na.rm; so use below instead
    # weighted.mean(comparison$medincE, as.numeric(comparison$Proportion))
    
    # manual weighted means
    with(
        # prevent NAs from enlarging denominator
        comparison |> filter(!is.na(medage)), 
        sum(medage * weights, na.rm = TRUE) / 
            sum(weights, na.rm = TRUE))
}
# test
pbarea_weights_medage("02X", incageacs |> filter(Year == 2015))





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
                             popacs |> filter(Year == year)),
               Age = map_dbl(policebeats$Beat,
                             funcs[[2]],
                             incageacs |> filter(Year == year)),
               Income = map_dbl(policebeats$Beat,
                                funcs[[3]],
                                incageacs |> filter(Year == year)),
        ) |>
        left_join(map_df(policebeats$Beat,
                         funcs[[4]],
                         raceacs |> filter(Year == year)),
                  join_by(Beat)) |>
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
                             popacs |> filter(Year == year)),
               Age = map_dbl(neighbs$Neighborhood,
                             funcs[[2]],
                             incageacs |> filter(Year == year)),
               Income = map_dbl(neighbs$Neighborhood,
                                funcs[[3]],
                                incageacs |> filter(Year == year)),
        ) |>
        left_join(map_df(neighbs$Neighborhood,
                         funcs[[4]],
                         raceacs |> filter(Year == year)),
                  join_by(Neighborhood)) |>
        mutate(Year = as.integer(year),
               .before = Neighborhood)
}

# put all together for each
demopbcorrect <- map_df(years, pbadd_stats)
demone <- map_df(years, nadd_stats)
# rough compute time: 20 minutes :(
# TODO: see if clustering reduces compute time
# TODO: explore whether sfheaders can do st_area() type alg or,
#   alternatively, whether loading shps with it goes faster

write_rds(demopb, "demopb.rds")
write_rds(demone, "demone.rds")


# TODO: populations for Shafter don't match added races. See 
# 2022 and huge dropoffs between 2012-2013; looks like tract
# numbers are much higher. maybe the functions somehow computed
# block group like tract, such as adding up the number of tracts
# instead of the number of block groups. Other vars seem fine
# 
demone[demone$Neighborhood %in% shafter, ] |> View()



