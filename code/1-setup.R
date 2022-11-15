
# Header ------------------------------------------------------------------

# This project summarizes data from the NEFSC Observer Program 
# Data will be used to inform the HMS SAFE Report annually
# this doc has some metadata: 
# G:\SF1\SHARKS\Observers\Data\NE Observer Data\NE Smooth Dogfish Data\2018\Smooth_dogfish.docx

# we will go back to 2018 to ensure consistent numbers 

# Ben Duffin 
# 11/10/2022


## QUESIONST

# HAILWT converted to DW LBS? 
# 2018 - way off
# 2019 - close when not converted
# 2020 - close when converted, but still high? 

# Requirements ------------------------------------------------------------



# Methods -----------------------------------------------------------------

# 1.43 used for round to DW conversion



# Libraries ---------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
library(here)


# File Struture -----------------------------------------------------------

# set up the file strucutre
dirs <- c("code", "data", "output")

for (i in 1:length(dirs)){
  if(dir.exists(dirs[i]) == FALSE){
    dir.create(dirs[i])
  }
}

rm(dirs, i)

## ADD DIRS TO GITIGNORE
# just new lines 
# data/
# output/

# Load Data ---------------------------------------------------------------

## Smooth Dogfish (Section 5.3.7)
# base path for Smooth Dogfish
smoothdog_path <- "G:/SF1/SHARKS/Observers/Data/NE Observer Data/NE Smooth Dogfish Data/"

#ne18 <- read_xlsx(paste0(smoothdog_path, "2018/2018_smooth_dogfish.xlsx")) # omit - only doing 3 years
ne19 <- read_xlsx(paste0(smoothdog_path, "2019/2019_smooth_dogfish_trips.xlsx"))
ne20 <- read_xlsx(paste0(smoothdog_path, "2020/2020_smooth_dogfish_trips.xlsx"))
ne21 <- read_xlsx(paste0(smoothdog_path, "2021_smooth_dogfish_trips.xlsx"))


## Non-HMS Target Fisheries (Section 5.4.2) 
# G:\SF1\SHARKS\Observers\Data\NE Observer Data
nthms <- read_xlsx("G:/SF1/SHARKS/Observers/Data/NE Observer Data/2021_HMS.xlsx", 
                   sheet = "Export Worksheet")

# Smooth Dogfish   ---------------------------------------------------------

# how different are all the names
setdiff(names(ne18), names(ne19))
setdiff(names(ne19), names(ne18))

setdiff(names(ne20), names(ne21))


# include year for all datasetsx
year <- 2019 # starting year

ne_list <- list(ne19, ne20, ne21)

for (i in seq_along(ne_list)) {
  ne_list[[i]][ , dim(ne_list[[i]])[2]+1] = as.character(rep(year), dim(ne_list[[i]])[1])
  year <- year + 1
}

# fix name 
ne_list <- lapply(ne_list, function(x) {
  colnames(x)[grepl("\\.", colnames(x))] = "year"
  x
})

lapply(ne_list, function(x) head(x$year))

# combine
ne <- bind_rows(ne_list)

# write all these data to local folder 
write_xlsx(ne, 
           here::here("output", paste0("NE_obs_2019-2021_", Sys.Date(), ".xlsx")))



# Non-Target HMS  ---------------------------------------------------------

# looking at catch of HMS in NON-SMOOTHDOGFISH, GILLNETS
# All Interactions, but reporting:
  # total number caught
  # number kept by PERCENT

glimpse(nthms)

table(nthms$GEARNM)
table(nthms$GEARNUM)

# write to local folder 
write_xlsx(nthms, 
           here::here("output", paste0("NE_obs_non-target_HMS_", Sys.Date(), ".xlsx")))
