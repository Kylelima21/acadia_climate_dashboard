## Tidy sea level data

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in file

rlr_data <- read.table("test_sea_data/525.rlrdata", header = TRUE, fill = TRUE)

#-----------------------#
####    Data Manip   ####
#-----------------------#