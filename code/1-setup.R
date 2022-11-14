
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

# base path
smoothdog_path <- "G:/SF1/SHARKS/Observers/Data/NE Observer Data/NE Smooth Dogfish Data/"

ne18 <- read_xlsx(paste0(smoothdog_path, "2018/2018_smooth_dogfish.xlsx"))
ne19 <- read_xlsx(paste0(smoothdog_path, "2019/2019_smooth_dogfish_trips.xlsx"))
ne20 <- read_xlsx(paste0(smoothdog_path, "2020/2020_smooth_dogfish_trips.xlsx"))
ne21 <- read_xlsx(paste0(smoothdog_path, "2021_smooth_dogfish_trips.xlsx"))



# Prototyping -------------------------------------------------------------

# 2018 = 105,942; 2019 = 83426; 2020 = 4406 (lb dw)
head(ne18)
sum(ne18$HAILWT) # weight 
table(ne18$DRFLAG) # grade

ne18$lb_dw <- ifelse(ne18$DRFLAG == "ROUND", ne18$HAILWT / 1.43, 
                     ne18$HAILWT)
sum(ne18$lb_dw)

