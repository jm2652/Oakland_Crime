# TODO: 
# • run through and prune code
# • check how many of the main categories are covered (can plot logs)
#   and add more

library(dplyr)
library(readr)
library(stringr)
library(arrow)

# Import crime
crime1.0 <- read_rds("crime1.0.rds")



distcrimes <- crimexyDT |> 
    group_by(CrimeType) |> 
    count() |> 
    arrange(desc(n))



#TODO: remove dontuse; then refactor vectors to simplify
# Function to create vectors of crime strings
mk_str_vec <- function(corr = NULL, yes1, yes2 = NULL, no1 = NULL, no2 = NULL, vector = NULL) {
    if (is.null(vector))  vec <-  c()
    if (!is.null(vector)) {
        vec <-  vector
    }
    
    # two exception strings
    if (!is.null(no1) & !is.null(no2) & is.null(yes2)) {
        vec <- vec |> append(
            distcrimes$CrimeType[str_detect(
                                         distcrimes$CrimeType, yes1) &
                                     !str_detect(
                                         distcrimes$CrimeType, no1) &
                                     !str_detect(
                                         distcrimes$CrimeType, no2)])
        
    } else if (!is.null(no1) & !is.null(no2) & !is.null(yes2)) {
        vec <- vec |> append(
            distcrimes$CrimeType[str_detect(
                                         distcrimes$CrimeType, yes1) &
                                     str_detect(
                                         distcrimes$CrimeType, yes2) &
                                     !str_detect(
                                         distcrimes$CrimeType, no1) &
                                     !str_detect(
                                         distcrimes$CrimeType, no2)])
    } else if (!is.null(no1) & is.null(yes2)) {
        vec <- vec |> append(
            distcrimes$CrimeType[str_detect(distcrimes$CrimeType, yes1) &
                                     !str_detect(distcrimes$CrimeType, no1)])
    } else if (!is.null(no1) & !is.null(yes2)) {
        vec <- vec |> append(
            distcrimes$CrimeType[str_detect(distcrimes$CrimeType, yes1) &
                                     str_detect(distcrimes$CrimeType, yes2) &
                                     !str_detect(distcrimes$CrimeType, no1)])
    }  else if (is.null(no1) & is.null(no2) & !is.null(yes2)) {
        vec <- vec |> append(
            distcrimes$CrimeType[str_detect(distcrimes$CrimeType, yes1) &
                                     str_detect(distcrimes$CrimeType, yes2)])
    } else {    
        vec <- vec |> append(
            distcrimes$CrimeType[str_detect(distcrimes$CrimeType, yes1)])
    }
    if (!is.null(corr)) {print(paste("Vector: ", corr))}
    print(vec)
}


# helper function "search string" for searching erroneous strings
ss <- function(string) {
    distcrimes$CrimeType[str_detect(distcrimes$CrimeType, string)]
}

# TODO: find way to remove duplicates
# helper function "search Description" for searching erroneous strings
ssde <- function(string) {
    crimexyDT$Description[str_detect(crimexyDT$Description, string)]
}
ssde("MURDER")

# helper function "crosscheck" for crosschecking description
cch <- function(string) {
    crimexyDT |> filter(CrimeType == string) |> select(Description)
}


### Create vectors for major crimes

# Arson	
arson <- mk_str_vec(corr = "ARSON",
                    yes1 = "ARSON")
arson <- mk_str_vec(vector = arson,
                    yes1 = "FIRE",
                    yes2 = "SET",
                    no1 = "PROTECTION",
                    no2 = "ALARM") |> 
    append(ss("CAUSING FIRE"))
# l.arson <- l.arson |> append("arson")

# Auto burglary
auto_burglary <- mk_str_vec(corr = "AUTO BURGLARY",
                            yes1 = "BURG",
                            yes2 = "AUTO",
                            no1 = "RESIDENT",
                            no2 = "COMMERCIAL")

attempt_auto_burglary <- mk_str_vec(corr = "ATTEMPT AUTO BURGLARY",
                                    yes1 = "ATTEMPT",
                                    yes2 = "BURGLARY-AUTO") |> 
    append("BRGLARY")

l.auto_burg <- c(auto_burglary, attempt_auto_burglary)
# l.auto_burg <- l.auto_burg |> append("auto burglary")

# Burglary	
burglary <- mk_str_vec(corr = "BURGLARY",
                       yes1 = "BURG",
                       no1 = "COMM",
                       no2 = "AUTO")
comm_burglary <- "BURG - RESIDENTIAL"
l.burglary <- c(burglary, l.auto_burg, comm_burglary)
# l.burglary <- l.burglary |> append("burglary")

# Carjacking
carjacking <- mk_str_vec(corr = "CARJACKING",
                         yes1 = "CARJ") |> 
    append(ss("CAR J"))
# carjacking <- carjacking |> append("carjacking")

# Disturbing the Peace	
disturb_peace <- mk_str_vec(corr = "DISTURB PEACE",
                            yes1 = "DISTURB")
# disturb_peace <- disturb_peace |> append("disturb peace")

# Manslaughter
manslaughter <- ss("MANSLA")
# manslaughter <- manslaughter |> append("manslaughter")

# Drug sales
sale_narcotics <- mk_str_vec(corr = "SALE NARCOTICS",
                             yes1 = "NARC",
                             yes2 = "S[AE]L[LE]") |> 
    append(c("ATTEMPT/CONSPIRACY:DRUGS", 
             "BUSINESS SELL/ETC DRUG PARAPHERNALIA TO PERSON UND 18",
             "FURNISH DANGEROUS DRUG/DEVICE WITHOUT PRESCRIPTION",
             "UNAUTHORIZED REFILL/ETC DANGEROUS DRUG PRESCRIPTION/DEVICE"))

sale_marijuana <- mk_str_vec(corr = "SALE MARIJUANA",
                             yes1 = "MAR",
                             yes2 = "S[AE]L[LE]")
l.drugsale <- c(sale_marijuana, sale_narcotics)
# l.drugsale <- l.drugsale |> append("drug sales")

# Drugs / Alcohol Violations
# #TODO: finish this
alcohol <- ss("ALC")[1:19]
possess_narcotics <- mk_str_vec(corr = "POSSESS NARCOTICS",
                                yes1 = "NARC",
                                yes2 = "POSS",
                                no1 = "S[AE]L[LE]")
# ss("DRUG")

l.drugsalc <- c(alcohol, possess_narcotics)
# l.drugsalc <- dui |> append("drugs/alcohol")

# DUI	
dui <- ss("DUI")
# dui <- dui |> append("dui")

# Fraud	
fraud <- ss("FRAUD")
# fraud <- fraud |> append("fraud")



# TODO: Rework homicide vector
crime |> filter(CrimeType == "HOMICIDE" & year(DateTime) == 2021) |> 
    count(CrimeType)
# this gives a clue; will have to rework homicide
crime |> 
    filter(CrimeType == "HOMICIDE" & Description != "SC UNEXPLAINED DEATH") |> 
    sample_n(20) |> select(Description)
crime |> 
    filter(CrimeType == "HOMICIDE" & Description != "SC UNEXPLAINED DEATH") |>
    count()


# Homicide	

attempt_hom <- mk_str_vec(corr = "ATTEMPT MURDER",
                          yes1 = "ATT",
                          yes2 = "MURD")
homicide <- mk_str_vec("HOMICIDE",
                       yes1 = "HOMI") |> 
    append(ss("^MURDER"))
l.homicide <- c(attempt_hom, homicide)
#TODO: reconsider whether to put attempt_hom in homicide or assault
# l.homicide <- l.homicide |> append("homicide")

# Motor Vehicle Theft
attempt_auto_theft <- mk_str_vec(corr = "ATTEMPT AUTO THEFT",
                                 yes1 = "ATTEMPT VEH")
attempt_auto_theft <- mk_str_vec(vector = attempt_auto_theft,
                                 yes1 = "ATTEMPT",
                                 yes2 = "10851") |> 
    append("ATTEMPTED STOLEN") # Description: "ATTEMPT VEHICLE THEFT-AUTO"

auto_theft <- mk_str_vec(corr = "AUTO THEFT",
           yes1 = "AUTO",
           yes2 = "THEF") |> append(
               mk_str_vec(corr = "AUTO THEFT",
                          yes1 = "VEH",
                          yes2 = "THEF",
                          no1 = "FALSE")
           )
auto_theft <- mk_str_vec(vector = auto_theft,
                            yes1 = "VEH",
                            yes2 = "STOL",
                            no1 = "ATTEMPT")
auto_theft <- mk_str_vec(vector = auto_theft,
                            yes1 = "10851",
                            no1 = "ATTEMPT") |> 
    append(c("AUTO HTEFT", "JOYRIDING-AUTO"))

# TODO/NOTE: Took shortcut resulting in list with index that starts over,
# plus duplicate auto theft
l.auto_theft <- c(attempt_auto_theft, auto_theft)
# l.auto_theft <- l.auto_theft |> append("auto theft")

# Robbery	
robbery <- ss("ROBB") |> append("ROB")
home_invasion <- mk_str_vec(corr = "HOME INVASION",
                            yes1 = "ROBB",
                            yes2 = "INHAB") |> 
    append(ss("ROBBERY - RESIDENTIAL")) |> 
    append("BURGLARY-FORCIBLE ENTRY")
l.robbery <- c(robbery, home_invasion)
# l.robbery <- l.robbery |> append("robbery")

# Sex Crimes	
sex_offense <- ss("SEX")
sex_reg_fail <- sex_offense[sex_offense %in% sex_offense[str_detect(sex_offense, "REG")]]
sex_offense <- sex_offense[!sex_offense %in% sex_offense[str_detect(sex_offense, "REG")]]
#TODO: Added this in from originals; double check
sexual_assault <- mk_str_vec(corr = ss("SEX")[2], 
                             yes1 = "SEX", 
                             no1 = "MINOR", 
                             no2 = "CHILD") |> 
    append(ss("FOREIGN OBJECT")) |> 
    append(c("ASSAULT TO COMMIT RAPE", "ASSAULT TO COMMIT ORAL COPULATION",
             "ASSAULT TO COMMIT SODOMY"))
l.sex_offense <- c(sex_offense, sex_reg_fail, sexual_assault)
# l.sex_offense <- l.sex_offense |> append("sex offense")

# Assault	
assault <- mk_str_vec(corr = "ASSAULT",
                      yes1 = "ASSAULT",
                      no1 = "SEX",
                      no2 = "WEAP") |> 
    append("MISDEMEANOR ASSAULT") |> 
    append("EXAASSAULT") |> 
    append("ASAULT") |> 
    append("ASST") |> 
    append("ASSULT") |> 
    append("AST")
# manually remove bc not enough no args
assault <- assault[!assault %in% assault[str_detect(assault, "POLICE")] &
                       !assault %in% assault[str_detect(assault, "PEACE")]]
assault <- assault[
    !assault %in% assault[str_detect(
        assault,"BATTERY ON NONCOHABITATING FORMER SPOUSE/ETC")]] |> 
    append(ss("BATTERY")) |> 
    append("BAT W/SERIOUS BODILY INJ") |> 
    append(ss("\\SASSAULT"))

assault_w_weapon <- mk_str_vec(corr = "ASSAULT W/ WEAPON",
                               yes1 = "ASSAULT",
                               yes2 = "WEAP",
                               no1 = "SEX")
assault_w_weapon <- mk_str_vec(vector = assault_w_weapon,
                               yes1 = "FIREA",
                               yes2 = "ASSAULT") |> 
    append("ASSAULT WITH STUN GUN/TASER") |> 
    append(ss("ASLT"))
assault <- assault[!assault %in% sexual_assault]

assault_le <- mk_str_vec(corr = "ASSAULT POLICE",
                         yes1 = "BATTERY AGAINST",
                         yes2 = "PO") |> 
    append(ss("BATTERY ON PEACE OFFICER")) |> 
    append("ASSAULT W/FIREARM ON PEACE OFFICER/FIREFIGHTER") |> 
    # append("ASSAULT ON PEACE OFF/FF/ETC") |> 
    append("THEFT/BATTERY ON OFFICE")
# wrap in vector
l.assault <- c(assault, assault_le, assault_w_weapon)
# l.assault <- l.assault |> append("assault")

# Theft / Larceny	
theft <- mk_str_vec(corr = "THEFT",
                    yes1 = "THEFT",
                    no1 = "VEHICLE",
                    no2 = "MAIL") |> 
    append(ss("\\STHEFT")) |> 
    append(ss("STEAL"))
theft <- theft[!theft %in% theft[str_detect(theft, "ALTHEFT")]]
theft <- theft[!theft %in% theft[str_detect(theft, "CHILD STEALING")]]
# #TODO: separate
# crime |> filter(CrimeType == "ALTHEFT") |> select(Description)
# #> 1 BURGLARY-AUTO
# #> 2 THEFT     
# theft <- theft |> append("theft")


# Vandalism	
vandalism <- mk_str_vec(corr = "VANDALISM",
                        yes1 = "VAND",
                        no1 = "INTENT") |> 
    append(ss("VND"))
# vandalism <- vandalism |> append("vandalism")

# Weapons	
concealed_weapon <- mk_str_vec(corr = "CONCEALED WEAPON",
                               yes1 = "CONCEAL",
                               no1 = "EVIDENCE")
possess_weapon <- mk_str_vec(corr = "POSSESS WEAPON",
                             yes1 = "WEAP",
                             no1 = "CONCE",
                             no2 = "W/") |> 
    append(ss("ILLEGAL POSS ASSAULT WPN"))
possess_weapon <- possess_weapon[
    !possess_weapon %in% possess_weapon[
        str_detect(possess_weapon, "ROBB") |
            str_detect(possess_weapon, "CARJACK") |
            str_detect(possess_weapon, "ASSAULT")]] |> 
    append(c("MANUFACTURES/DISTRIBUTE/TRANSFER/ETC ANY ASSAULT WEAPON",
           "ASSAULT WEAPON",
           "POSSESSES ANY ASSAULT WEAPON"))
possess_weapon <- mk_str_vec(vector = possess_weapon,
           yes1 = "GUN",
           no1 = "ASSAULT")
possess_weapon <- mk_str_vec(vector = possess_weapon,
                             yes1 = "KNIFE",
                             no1 = "ROBB",
                             no2 = "CARJAC")
possess_weapon <- possess_weapon[!possess_weapon %in% possess_weapon[
    str_detect(possess_weapon,"ATTEMPTED MURDER-KNIFE")]]


exhibit_firearm <- mk_str_vec(corr = "EXHIBIT FIREARM",
                              yes1 = "EXHI")
l.weapon <- c(possess_weapon, exhibit_firearm)
# l.weapon <- l.weapon |> append("weapons")

# Traffic
traffic <- mk_str_vec(corr = "TRAFFIC",
                      yes1 = "TRAFF",
                      no1 = "TRAFFICK")
# traffic <- traffic |> append("traffic")

# TODO: warrant
ss("WARRANT")
ss("WRT")
ss("WRN")
ss("WNT")

# TODO
# Sex Offender NA*	

# TODO
# Sexual Predator NA*	

l.major <- c(l.drugsalc, arson, l.assault, l.auto_burg, l.auto_theft, l.burglary,
             l.drugsale, l.drugsalc, l.homicide, l.robbery, l.sex_offense, l.weapon,
             carjacking, disturb_peace, dui, fraud, vandalism, manslaughter, theft,
             traffic)






# TODO: figure out difference between & and | in str_detect

# # NOTE: Example of bad discrepancies btwn CrimeType & Description
# crime |> filter(Description == "BURGLARY-AUTO" &
#                     !(CrimeType %in% auto_burglary))


# ss("ILLEGAL POSS ASSAULT WPN")



# TODO: See mincrime. Add "STOLEN" and "SHOPLIFT" and check MARIJUANA.
# Add "FORCIBLE RAPE" to sexual_assault.
# add domestic violence.
# add drunkenness to alcohol










write_rds(l.major, "l.major.rds")
l.major <- read_rds("l.major.rds")

# Refactor Crimes ---------------------------------------------------------

# TODO: Check if it's necessary to make two dfs and bind them


# NEW
# separate out major crimes
majcrime <-  crimexyDT |>
    filter(CrimeType %in% l.major)


# Factorize CrimeTypes in majorcrime
majorcrime <- majcrime |> 
    mutate(
        CrimeType = fct_collapse(CrimeType,
                                 "arson" = arson,
                                 "assault" = l.assault,
                                 "auto theft" = l.auto_theft,
                                 "auto burglary" = l.auto_burg,
                                 "carjacking" = carjacking,
                                 "burglary" = l.burglary,
                                 "disturb peace" = disturb_peace,
                                 "drug sales" = l.drugsale,
                                 "drugs/alcohol" = l.drugsalc,
                                 "dui" = dui,
                                 "fraud" = fraud,
                                 "homicide" = l.homicide,
                                 "manslaughter" = manslaughter,
                                 "robbery" = l.robbery,
                                 "sex offense" = l.sex_offense,
                                 "theft" = theft,
                                 "traffic" = traffic,
                                 "weapons" = l.weapon,
                                 "vandalism" = vandalism,
                                 "traffic" = traffic
        ))
# TODOLAST: To see whether can automate the manual entry of categories as the 
# name of the vector.
# https://stackoverflow.com/questions/67142718/embracing-operator-inside-mutate-function
# #https://stackoverflow.com/questions/14577412/how-to-convert-variable-object-name-into-string
# deparse((substitute(home_invasion)))

# majorcrime |> filter(CrimeType == "PETTY THEFT") |> select(CrimeType)
# majorcrime |> filter(CrimeType == "traffic") |> select(CrimeType)

# separate out "minor" (in that minor == !major) crimes
"%!in%" <- function(x,y) {!(x %in% y)}
mincrime <- crimexyDT |> filter(CrimeType %!in% l.major)
crimexyDT |> colnames()
# Lump all non-major crimes into one factor
minorcrime <- mincrime |> mutate(
    CrimeType = fct_lump_n(CrimeType, n = 0)
)

mincrime |> 
    group_by(CrimeTypeOrig) |>
    distinct() |> 
    count() |> 
    arrange(desc(n)) |> 
    View()
minorcrime |> sample_n(20)
minorcrime |>  # lump didn't work. TODO
    group_by(CrimeTypeOrig) |>
    distinct() |> 
    count() |> 
    arrange(desc(n)) |> 
    View()

# Combine
minmaj <- list(majorcrime, minorcrime)
# NEW: changed name crime2 to crime1.1
crimexyDT1.1 <- rbindlist(minmaj) |> data.table()






# NEW
rm(crime1.0)
# rm(c(ssde, mk_str_vec, cch, ss)) # check if c() syntax works
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 