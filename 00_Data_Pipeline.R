library(httr2)
library(jsonlite)
library(data.table)
library(readr)

# Using fread is way easier: didn't have to un-nest JSON.

# The weird calculated value field wasn't included this time; data had been updated
# so maybe that was why.

dtcrime <- fread("https://data.oaklandca.gov/resource/ppgh-7dqv.csv?$LIMIT=1150000")
# crimcsv <- req_perform(request(
    # "https://data.oaklandca.gov/resource/ppgh-7dqv.csv?$LIMIT=1150000")) # httr2 doesn't do CSV
dtcrime |> View()

# # perfecting: custom requests based on latest size
# https <- "sasdf"
# url <- "https://data.oaklandca.gov/resource/ppgh-7dqv.json"
# rowlimit <- 10
# url2 <- paste(c(url, rowlimit, sep = ""))
# url2



# perform request to City of Oakland website
crim <- req_perform(request(
    "https://data.oaklandca.gov/resource/ppgh-7dqv.json?$LIMIT=1150000"))
crime <- tibble(c = resp_body_json(crim))

# unnest listcols
crime <- crime |> 
    unnest_wider(c) |> 
    unnest_wider(location) |>
    unnest_wider(coordinates, names_sep = "_") # TODO: figure out how to name
# remove ":@computed_region_w23w_jfhw" which is a strange artifact, probably from OPD's GIS
# TODO: see if still necessary with latest OPD dataset
crime <- crime[1:11]


# perfecting: print when the dataset was last updated
respheader <- crim |> resp_headers()
print(respheader$`Last-Modified`)


rm(crim)
write_rds(dtcrime, "crime_240103.rds")
