# Header ------------------------------------------------------------------

# This project summarizes data from the NEFSC Observer Program 
# Data will be used to inform the HMS SAFE Report annually
# this doc has some metadata: 
# G:\SF1\SHARKS\Observers\Data\NE Observer Data\NE Smooth Dogfish Data\2018\Smooth_dogfish.docx

# we will go back to 2018 to ensure consistent numbers 

# Ben Duffin 
# 11/10/2022


## QUESIONS

# HAILWT converted to DW LBS? 
# 2018 - way off
# 2019 - close when not converted
# 2020 - close when converted, but still high? 

# 11/14/2022
# Table 5.51 - only info from the file at G:\SF1\SHARKS\Observers\Data\NE Observer Data?
# OMG so the NT HMS is just HMS that doesn't include smooth dogfish???***********

# DO NOT COMBINE

# Also, tab "lookup" on this file: G:\SF1\SHARKS\Observers\Data\NE Observer Data\2021_HMS_wCodesDescribedForSAFE_DOHB 
# has HMS gear cats 
# FISHDISP conversions (kept retained) 
# final status(alive dead = STATEND)


# Requirements ------------------------------------------------------------



# Methods -----------------------------------------------------------------

# 1.43 used for round to DW conversion



# Libraries ---------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
library(here)



# Load Data ---------------------------------------------------------------

# Load the Non-HMS Target data observed
nthms <- read_xlsx(here::here("output", "NE_obs_non-target_HMS_2022-11-15.xlsx"))

# Fish Dispositions from the NE 
# I added in a dummy (1=kept) for HMS ** BD 11/14/2022
fishdisp <- read_xlsx(here::here("data", "OBFISHDISP.xlsx"))
glimpse(fishdisp)

# bring in the observer species 
sp <- read_xlsx(here::here("data", "OBSPEC.xlsx"))

# non-dogfish species 
othShark <- subset(sp, grepl("SHARK", sp$COMNAME))



# Preprocessing -----------------------------------------------------------

## Indicate HMS Gears (according to )
table(nthms$GEARNM, nthms$GEARNUM)

# reclassify 
nthms$HMS_gear <- ifelse(grepl("DREDGE", nthms$GEARNM), "DREDGE", 
                         ifelse(nthms$GEARNM == "GILL NET, FIXED OR ANCHORED,SINK, OTHER/NK SPECIES", "SINK GILLNET", 
                                ifelse(grepl("DRIFT", nthms$GEARNM), "DRIFT GILLNET",
                                       ifelse(grepl("TRAWL", nthms$GEARNM), "TRAWL", 
                                              ifelse(grepl("POTS", nthms$GEARNM), "POTS", 
                                                     ifelse(grepl("HANDLINE", nthms$GEARNM), "HANDLINE",
                                                            ifelse(grepl("LONGLINE, BOTTOM", nthms$GEARNM), "BOTTOM LONGLINE", 
                                                                   NA)))))))
table(is.na(nthms$HMS_gear))
table(nthms$GEARNM, nthms$HMS_gear)

## Adding disposition = Kept/Discarded
table(nthms$FISHDISP)

nthms$HMS_disp <- ifelse(substr(nthms$FISHDISP, 1, 1) == '1', "KEPT",
                         ifelse(nthms$FISHDISP == "900", "UNKNOWN", 
                                "DISCARD"))
table(is.na(nthms$HMS_disp))
table(nthms$FISHDISP, nthms$HMS_disp)


## Add status 
table(is.na(nthms$STATEND))
table(nthms$STATEND)

nthms$HMS_status <- ifelse(nthms$STATEND == "1", "ALIVE", 
                           ifelse(nthms$STATEND == "0", "NA", 
                                  "DEAD"))

table(nthms$STATEND, nthms$HMS_status)


## Add trip.set 
nthms$trip.set <- paste0(nthms$TRIPID, ".", nthms$HAULNUM)
table(is.na(nthms$trip.set))

## Add Flag for HMS species 
sp <- sp %>%
  mutate(hms_sp = ifelse(grepl("SHARK", COMNAME), "1", 
                         ifelse(grepl("TUNA", COMNAME), "1", 
                                ifelse(grepl("SWORD", COMNAME), "1", 
                                       "0"))))

# Analysis ----------------------------------------------------------------

# number of trips 

# Prototyping -------------------------------------------------------------

targ_cols <- names(nthms)[grepl("TARGSPEC", names(nthms))]

t <- nthms %>% 
  filter_at(targ_cols, any_vars(. %in% othShark$NESPP4)) %>%
  mutate(sharkTarg = 1)
