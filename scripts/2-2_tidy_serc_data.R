## Tidy SERC data  

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

#Reading in CSV as a tibble

serc.data <- read.csv("data/raw_data/SERC_D2258_export_20241119.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   ####
#-----------------------#


