#Setup

# ----Libraries needed for the analysis ----

# Data management
library(tidyverse)
library(stringi)
library(readxl)
library(sf)
library(colorspace)
library(ggtext)
library(extrafont)
library(readr)
library(janitor)
library(ggrepel)
library(tigris)
library(tidycensus)
library(tmap)
library(writexl)
library(plotly)
library(htmlwidgets)
library(haven)
library(patchwork)
library(lutz)
library(mapboxapi)
library(flextable)
library(gt)
library(ggbeeswarm)
library(ggpubr)
library(cowplot)
library(scales)
library(glue)
library(sRtr)
library(CDCAtlas)

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