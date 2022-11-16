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

# non-dogfish sharks
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

# remove blackfin tuna, etc
sp_rm <- c("4640", "4641", "4656", "4657", "4680", "4682")

# switch those to 0 for hms
sp$hms_sp <- ifelse(sp$NESPP4 %in% sp_rm, "0", sp$hms_sp)

hmsSp_codes <- unique(sp$NESPP4[sp$hms_sp == "1"])

nthms$hms_sp <- ifelse(nthms$NESPP4 %in% hmsSp_codes, "1", "0")

table(nthms$hms_sp)

table(nthms$COMNAME[nthms$hms_sp == "0"]) # unknown tuna - I didn't include

# at least we know they are all hms species 

## Trips targeting species that ARE NOT sharks or dogfish
# target columns 
targ_cols <- names(nthms)[grepl("TARGSPEC", names(nthms))]

head(nthms[, targ_cols])

apply(nthms[, targ_cols], 2, function(x) sum(x == '0000', na.rm = T))

# need to determine if othShark in the target columns 
shark_targ_ind <- ifelse(apply(nthms[, targ_cols], 1, function(x) 
   any(x %in% unique(othShark$NESPP4))), 
                           "1", "0")
table(shark_targ_ind)
# add to the data frame
nthms$shark_targ <- shark_targ_ind

# Analysis ----------------------------------------------------------------

# how many by gear type? 


## General info - trips, sets, n vessels - gillnets
(
nt_summary <- nthms %>%
  summarize(n_trips = n_distinct(TRIPID), 
            n_sets = n_distinct(trip.set), 
            n_vessels = n_distinct(HULLNUM1))
)

## non-target species retained - total n and kept%
(
nt_spr <- nthms %>% 
  filter(HMS_gear %in% c("DRIFT GILLNET", "SINK GILLNET")) %>%
  group_by(COMNAME, HMS_disp) %>%
  summarize(n_tot = n(), 
            n_vessels = n_distinct(HULLNUM1)) %>%
  pivot_wider(names_from = HMS_disp, 
              values_from = c(n_tot, n_vessels),  
              values_fill = 0) %>%
  mutate(n_tot = sum(n_tot_DISCARD, n_tot_KEPT), 
         n_vessels = sum(n_vessels_DISCARD, n_vessels_KEPT), 
         kept_perc = n_tot_KEPT / n_tot) %>%
  select(COMNAME, n_tot_DISCARD, n_tot_KEPT, n_tot, kept_perc, n_vessels) %>%
  arrange(desc(n_tot))
)  

## drift gillnet only - sets, trip, vessels
(
  nt_dg_summary <- nthms %>%
    filter(HMS_gear == "DRIFT GILLNET") %>%
    summarize(n_trips = n_distinct(TRIPID), 
              n_sets = n_distinct(trip.set), 
              n_vessels = n_distinct(HULLNUM1))
    
)

## drift gillnet only - catch from NOT TARGETTING SHARK OR DOGFISH
(
  nt_dg_catch <- nthms %>%
    filter(HMS_gear == "DRIFT GILLNET") %>%
    group_by(COMNAME) %>%
    summarize(n_caught = n()) %>%
    arrange(desc(n_caught))
)

## sink gillnet only - sets, trips, vessels
(
  nt_sg_summary <- nthms %>%
    filter(HMS_gear == "SINK GILLNET") %>%
    summarize(n_trips = n_distinct(TRIPID), 
              n_sets = n_distinct(trip.set), 
              n_vessels = n_distinct(HULLNUM1))
  
)
## sink gillnet only - catch from NOT TARGGETING SHARK OR DOGFISH
(
  nt_sg_catch <- nthms %>%
    filter(HMS_gear == "SINK GILLNET") %>%
    group_by(COMNAME) %>%
    summarize(n_caught = n()) %>%
    arrange(desc(n_caught))
)


# Prototyping -------------------------------------------------------------

targ_cols <- names(nthms)[grepl("TARGSPEC", names(nthms))]

t <- nthms %>% 
  filter_at(targ_cols, any_vars(. %in% othShark$NESPP4)) %>%
  mutate(sharkTarg = 1)
