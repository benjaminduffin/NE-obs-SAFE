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

# Libraries ---------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
library(here)



# Load Data ---------------------------------------------------------------


# NE TRIPS data
ne <- read_xlsx(here::here("output", "NE_obs_2019-2021_2022-11-14.xlsx"))
glimpse(ne)

# Fish Dispositions from the NE 
# I added in a dummy (1=kept) for HMS ** BD 11/14/2022
fishdisp <- read_xlsx(here::here("data", "OBFISHDISP.xlsx"))
glimpse(fishdisp)

# bring in the observer species 
sp <- read_xlsx(here::here("data", "OBSPEC.xlsx"))

# Preprocessing -----------------------------------------------------------

# add in conversion factor 
ne$conversion_factor <- 1.43

# pull out HMS gears - we want GILL NETS
table(ne$GEARNM, ne$year) # trawl included in the first year here - 2019
table(is.na(ne$GEARNM))
table(ne$YEAR, ne$year)

# reclassify gear
ne$HMS_gear <- ifelse(ne$GEARNM == "TRAWL,OTTER,BOTTOM,FISH", "N", "Y")
table(ne$HMS_gear)

# also need to consider the disposition codes 
table(ne$FISHDISP)

# merge in the descriptions 
setdiff(ne$FISHDISP, fishdisp$FISHDISP)
setdiff(fishdisp$FISHDISP, ne$FISHDISP)
ne <- merge(ne, fishdisp[, c("FISHDISP", "FISHDISPDESC", "HMS_disp_BD")], 
            all.x = T)


table(ne$FISHDISPDESC[ne$COMNAME == "DOGFISH, SMOOTH"], # all good
      ne$FISHDISP[ne$COMNAME == "DOGFISH, SMOOTH"])

# reclassify -- updated 11/15/2022 -- 
# HB provided table w/ anything starts with 1 is kept, else discard
#ne$HMS_Disp <- ifelse(is.na(ne$HMS_disp_BD), "DISCARD", "KEPT")
ne$HMS_Disp <- ifelse(substr(ne$FISHDISP, 1, 1) == '1', "KEPT", "DISCARD")
table(ne$HMS_Disp)
table(is.na(ne$HMS_Disp))

table(ne$FISHDISPDESC, ne$HMS_Disp)


# indicate if they are smooth dogfish sets 
dogfishSp <- subset(sp, grepl("MUSTELUS", sp$SCINAME))
table(ne$NESPP4[ne$COMNAME == "DOGFISH, SMOOTH"])

dogfishSp[dogfishSp$NESPP4 %in% ne$NESPP4, ] # so only 3501 and 3511
# going to make a call and include both

# other shark species 
othShark <- subset(sp, grepl("SHARK", sp$COMNAME))


# add in protected species 
species <- unique(ne$COMNAME)
prot_species <- species[grepl("SEAL|TURTLE|SAW|STURG|DOLPHIN|PORPOISE|WHALE|BIRD", species)]


# Analysis ----------------------------------------------------------------

# protected species by year
(
gn_prot_catch <- ne %>%
  filter(COMNAME %in% prot_species) %>%
  group_by(YEAR, HMS_gear, COMNAME) %>%
  summarize(n_caught = n(), 
            n_vessels = n_distinct(HULLNUM1), 
            n_trips = n_distinct(TRIPID))
)

