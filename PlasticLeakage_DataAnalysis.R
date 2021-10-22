######## Data Import ########
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##
#############################



#### SETUP ####

# install required packages (if not installed yet)
packagelist <- c("tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'D:/Documents/WWF_PlastikLeakage_Vietnam/data'



####  DATA IMPORT ####

## data downloaded for time period: 01.10.2016-01.10.2021


#### Precipitation (daily)
 
## 1. NOAA GHCN (daily)
# downloaded from: https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/locations/FIPS:VM/detail
# downloaded in metric units (mm)
prcp_noaa <- read_csv(paste(dir, "/2755974.csv", sep = ""))



## 2. Meteostat (daily)
## downloaded via python API




#### DATA CLEANING ####

#### Precipitation

## 1. NOAA GHCND
## remove not needed column
prcp_noaa$PRCP_ATTRIBUTES <- NULL

## remove NA values (where no precipitation value recorded)
prcp_noaa <- prcp_noaa[!is.na(prcp_noaa$PRCP),]
  


#### Wind (hourly)



#### DATA CLEANING ####