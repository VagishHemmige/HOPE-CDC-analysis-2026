#Setup

# ----Libraries needed for the analysis ----

# Data management
library(tidyverse)
library(stringi)
library(glue)

#Core library for the SRTR
library(sRtr)

#Data import libraries
library(readxl)
library(readr)
library(haven)
library(mapboxapi)
library(CDCAtlas)

#Geographic libraries
library(sf)
library(tigris)
library(tidycensus)
library(tmap)
library(lutz)

#Table libraries
library(flextable)
library(gt)

#Plotting
library(colorspace)
library(ggtext)
library(extrafont)
library(janitor)
library(ggrepel)
library(writexl)
library(plotly)
library(htmlwidgets)
library(patchwork)
library(ggbeeswarm)
library(ggpubr)
library(cowplot)
library(scales)




# ----Define constants----

organ_list<-c("liver", "kidney", "heart", "lung", "pancreas")
year_list<-c("2017", "2022")
distance_list=c("50mile", "60min", "100mile", "200mile")

#List states which are continental
continental_fips <- c(
  "01", "04", "05", "06", "08", "09", "10", "11", "12", "13", "16",
  "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
  "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
  "37", "38", "39", "40", "41", "42", "44", "45", "46", "47",
  "48", "49", "50", "51", "53", "54", "55", "56"
)

#For isochrone analysis, we need to account for time zones in order to calculate driving time at 7 AM on the specific date

#List US continental time zones

timezones<-c("America/New_York","America/Chicago","America/Denver","America/Los_Angeles","America/Phoenix",
             "America/Indiana/Indianapolis","America/Kentucky/Louisville","America/Detroit")

reference_date<-list()

# Specific date to anchor the time: First Monday in March of both years
reference_date[["2022"]] <- as.Date("2022-03-07")
reference_date[["2017"]] <- as.Date("2017-03-06")
