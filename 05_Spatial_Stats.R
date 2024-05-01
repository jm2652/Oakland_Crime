library(readr)
library(dplyr)
library(purrr)
library(terra)
library(sf)
library(spdep)
library(tmap)


# graphics default state (in case graphics state is corrupted)
.pardefault <- par()


cgeo <- read_rds("Data/cgeo.rds")
cdt <- read_rds("Data/cDTclean.rds")
netot <- read_rds("Data/netotcrime.rds")
netot_norm <- read_rds("Data/netot_norm.rds")



# Getis-Ord GI* -----------------------------------------------------------

# Function that plots rasterized Getis-Ord GI*
# TODO: 
#   - replace plot with tmap;
#   - make it zoom consistently; 
#   - inspect spatial outlier
# that appears to be in Alameda
gilocal_crime <- function(crimename, year, demog, pixelsize) {
    # Subset demography file by year and crime
    c <- cgeo |> 
        filter(Crimetype == crimename &
                   Year == year)
    # Mark each incident with one for aggregation w/ sum()
    c$INCIDENT <- 1
    
    # Create raster
    box <- ext(demog)
    template <- rast(
        box,
        nrows = (box$ymax - box$ymin) * pixelsize,
        ncols = (box$xmax - box$xmin) * pixelsize)
    r <- rasterize(c, template, 
                   field = 'INCIDENT', fun = sum)

    # Create the list of neighbors
    r_grid = as.polygons(r) |> st_as_sf() |> as_Spatial()
    r_neighbors = poly2nb(r_grid)
    r_weighted_neighbors = nb2listw(r_neighbors, zero.policy=T)
    
    # # Perform the local G analysis (Getis-Ord GI*)
    r_grid$HOTSPOT = as.vector(localG(r_grid$sum, r_weighted_neighbors))
    
    # TODO: change breaks and colors
    breaks = c(-20, -1.96, -1, 1, 1.96, 20)
    palette=c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080")
    r_col = palette[cut(r_grid$HOTSPOT, breaks)]
    plot(r_grid["HOTSPOT"], col=r_col, border=NA,
         xlim=c(-122.3, -122.114), ylim=c(37.725, 37.87))
    demog$One <- 1
    plot(demog["One"], col=NA,  border="gray", add=T)
    # 
    # # Plot
    # # extract name of crime & convert to string
    # title <- c[1,1] |> st_drop_geometry()
    # title <- title$Crimetype |> as.character()
    # # borders
    # db <- tm_shape(demog) + tm_borders()
    # qtm(r_grid) + 
    #     db +
    #     tm_layout(
    #         main.title = paste(stringr::str_to_title(title), year),
    #         legend.position = c("left", "bottom"))
}
gilocal_crime("robbery", 2022, netot, 150)



# Intermediate function that runs Getis-Ord GI* on given demographic area,
# given crime, and given year
gi <- function(year, demog, crime) {
    crimename <- paste0(crime, "_GI")
    
    # filter year
    demog <- demog |> filter(Year == year)
    # extract numeric vector of normalized crime values
    c <- demog[{{crime}}] |> st_drop_geometry() |> pull()
    
    n <- poly2nb(demog)
    weights <- nb2listw(n, zero.policy = T)

    demog <- tibble::add_column(
        demog,
        !!crimename := as.vector(localG(c, weights)))
    demog

    # plot(demog[ncol(demog)])
}
gi(2020, netot_norm, "arson_norm") |> View()

# Combine all years into one dataframe for arson
timeframe <- range(netot_norm$Year)[1]:range(netot_norm$Year)[2]
# Test on different crimetypes
arson_GI <- map(timeframe, gi, netot_norm, "arson_norm") |> list_rbind()
sexoffense_GI <- map(timeframe, gi, netot_norm, "sex offense_norm") |> list_rbind()


# Create master list with dataframe for each crime
# list crimenames
crimenames_norm <- netot_norm |> st_drop_geometry() |> 
    select(contains("_norm")) |> colnames()
gi_map <- function(crime) {
    map(timeframe, gi, netot_norm, crime) |> list_rbind()
}
# Create list of full dataframes for each crime
gi_all_norm_list <- map(crimenames_norm, gi_map)
gi_all_norm_list |> write_rds("gi_all_norm_list.rds")


# Function that plots Getis-Ord GI* for given year and crime
gi_plot <- function(year, demog, crime) {
    crimename <- paste0(crime, "_GI")
    
    # filter year
    demog <- demog |> filter(Year == year)
    # extract numeric vector of normalized crime values
    c <- demog[{{crime}}] |> st_drop_geometry() |> pull()
    
    n <- poly2nb(demog)
    weights <- nb2listw(n, zero.policy = T)
    
    demog <- tibble::add_column(
        demog,
        !!crimename := as.vector(localG(c, weights)))
    
    plot(demog[ncol(demog)])
}
gi_plot(2021, netot_norm, "auto theft_norm")


# TODO: easily access for plotting. 
# Problem: plot_sf requires columns to be in [""] form
gi_all_norm_list[[1]][ncol(netot_norm)-1] |> View()
plot(gi_all_norm_list[[1]][crimenames_norm[1]])
crimenames_norm[1]



# TODO: understand relationship between netot and netot_norm totals. 
# Should work because *_norm is just a scalar shift, but
# they appear different
gi_plot(2020, netot_norm, "arson_norm")
gi_plot(2020, netot, "arson")
gi(2020, netot_norm, "arson_norm") |> select(arson_norm_GI) |> View()
gi(2020, netot, "arson") |> select(arson_GI) |> View()
netot_norm |> select(starts_with("arson")) |> View()


# TODO: animated tmap GIF
gi_all_norm_list <- read_rds("gi_all_norm_list.rds")




# Moran's I Statistic -----------------------------------------------------


# Define neighbors
neighbors <- poly2nb(as_Spatial(netot), queen = T)
weights <- nb2listw(neighbors, style = "W", zero.policy = T)


# Global Moran's I statistic ------------------------------------------------
# This compares changes in spatial autocorrelation across different years, 
# which tells us whether Oakland's crime remained concentrated in certain 
# neighborhoods, or became more spatially diffused.

# Set up weights
n18 <- netot_norm[netot_norm$Year == 2018, ]
n22 <- netot_norm[netot_norm$Year == 2022, ]

neighbors18 <- poly2nb(as_Spatial(n18), queen = T)
weights18 <- nb2listw(neighbors18, 
                      style = "W", zero.policy = T)
neighbors22 <- poly2nb(as_Spatial(n22), queen = T)
weights22 <- nb2listw(neighbors18, 
                      style = "W", zero.policy = T)


# Calculate global Moran's I statistic to compare spatial autocorrelation
# before and after COVID-19/defund police crimewave
moran_g_robbery_18 <- moran.test(n18$robbery_norm, weights18)
moran_g_robbery_22 <- moran.test(n22$robbery_norm, weights22)
print(moran_g_robbery_18) # .2799; weakly clustering
print(moran_g_robbery_22) # .23; slightly less clustering

# there was less robbery clustering in 2022, meaning it moved to neighborhoods
# that previously had lower crime
moran_g_robbery_18$estimate
moran_g_robbery_22$estimate

# Plot
moran.plot(n18$robbery_norm, weights18)
moran.plot(n22$robbery_norm, weights22)




# Local Moran's I statistic -----------------------------------------------


# Local Moran's I statistic
moran_local_robbery_18 <- localmoran(
    n18$robbery_norm, weights18)
moran_local_robbery_22 <- localmoran(
    n22$robbery_norm, weights22)
n18$Moran <- moran_local_robbery_18[, "Z.Ii"]
n22$Moran <- moran_local_robbery_22[, "Z.Ii"]

# Plot
breaks <- c(-10, -1, 1, 10)
plot(n18["Moran"],
     breaks = breaks,
     pal=colorRampPalette(c("navy", "gray", "red")))
plot(n22["Moran"],
     breaks = breaks,
     pal=colorRampPalette(c("navy", "gray", "red")))

# List outliers
n18 |> 
    arrange(Moran) |> 
    select(Neighborhood, Moran) |> 
    st_drop_geometry() |> 
    head()
n22 |> 
    arrange(Moran) |> 
    select(Neighborhood, Moran) |> 
    st_drop_geometry() |> 
    head()
# lots of overlap in neighborhoods, most in 
# downtown and deep east Oakland


# function: calculate local Moran I statistic to find outliers, and list
# outlier areas with lowest Z score
moran_local <- function(crime, year, demog, num_results) {
    # Subset demography file and crime file
    d <- demog[demog$Year == year,]
    c <- d[{{ crime }}] |> 
        st_drop_geometry() |> 
        pull()
    
    # Calculate weights
    neighbors <- poly2nb(as_Spatial(d), queen = T)
    weights <- nb2listw(neighbors,
                        style = "W", zero.policy = T)
    
    # Local moran test
    moran <- localmoran(c, weights)
    d$Moran <- moran[, "Z.Ii"]
    
    d |> 
        arrange(Moran) |>
        st_drop_geometry() |> 
        select(2, Moran) |> 
        head(num_results)
}
moran_local("robbery_norm", 2018, netot_norm, 5)

# Function: plot local Moran Z scores by area to find outliers 
# Blue areas have high Z scores; red areas have low, which
# means they are outliers compared to their neighbors (in
# any direction). Use the map to find priority (red) areas.
plot_moran_local <- function(crime, year, demog) {
    # Note: tried to just call moran_local() inside, but got
    # error: C stack usage is too close to the limit
    
    # Subset demography file and crime file
    d <- demog[demog$Year == year,]
    c <- d[{{ crime }}] |> 
        st_drop_geometry() |> 
        pull()
    
    # Calculate weights
    neighbors <- poly2nb(as_Spatial(d), queen = T)
    weights <- nb2listw(neighbors,
                        style = "W", zero.policy = T)
    
    # Local moran test
    moran <- localmoran(c, weights)
    d$Moran <- moran[, "Z.Ii"]
    
    # Plot
    breaks <- c(-10, -1.5, 1.5, 10)
    # blue spots have low Z score; red spots have high
    plot(d["Moran"],
         breaks = breaks,
         pal=colorRampPalette(c("red", "gray", "blue")))
}
plot_moran_local("robbery_norm", 2021, netot_norm)