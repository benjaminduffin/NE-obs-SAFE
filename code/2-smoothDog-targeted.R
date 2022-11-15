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

# Requirements ------------------------------------------------------------



# Methods -----------------------------------------------------------------

# 1.43 used for round to DW conversion



# Libraries ---------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
library(here)

# Load data ---------------------------------------------------------------

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


# pull out the target species 
head(ne$T_TARGET1)
head(ne$T_TARGET5)

targ_cols <- names(ne)[grepl("TARGET", names(ne))]


# for smooth dogfish only 
t <- ne %>% 
  filter_at(targ_cols, any_vars(. %in% dogfishSp$NESPP4)) %>%
  mutate(smoothDogflag = 1)

# any other shark targetted sets? -- None 
t1 <- ne %>% 
  filter_at(targ_cols, any_vars(. %in% othShark$NESPP4))


## Okay, so this data looks like it is only targeting smooth dogfish 

## SUBSET FOR ONLY SMOOTH DOGFISH?
smoothdog <- subset(ne, NESPP4 %in% dogfishSp$NESPP4)

# convert to dw lbs - only smooth dogfish 




# Summarize ---------------------------------------------------------------

# number of trips by year - TARGETTING SMOOTH DOGFISH
smoothdog$HULLNUM1

ne %>% 
  group_by(YEAR) %>% 
  summarize(n_trips = n_distinct(TRIPID), 
            n_vessels = n_distinct(HULLNUM1))
  

# number of sets by year 
ne %>% 
  mutate(trip.set = paste0(TRIPID, ".", HAULNUM)) %>%
  group_by(YEAR) %>%
  summarize(n_sets = n_distinct(trip.set), 
            n_vessels = n_distinct(HULLNUM1))




## Catch 
# convert to dwlbs
table(smoothdog$D_R)
smoothdog$dw_lbs <- ifelse(smoothdog$D_R == "ROUND", smoothdog$HAILWT / 1.43, 
                           smoothdog$HAILWT)

sd_catch <- smoothdog %>%
  group_by(YEAR, HMS_Disp) %>%
  summarize(tot_dwlbs = sum(dw_lbs), 
            n_vessels = n_distinct(HULLNUM1)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(annual_dwlbs = sum(tot_dwlbs)) %>%
  ungroup() %>%
  mutate(prop_disp = tot_dwlbs / annual_dwlbs)

sd_catch  

