#Loads data

# ----Load US state maps from census----
states_sf <- tigris::states(cb = FALSE, year = 2022)%>%
  select(NAME, STUSPS, geometry) %>%
  filter(!(NAME %in% c("Alaska", 
                       "Hawaii", 
                       "Puerto Rico", 
                       "United States Virgin Islands",
                       "Commonwealth of the Northern Mariana Islands"))) %>%
  rename(State=NAME)


#----Import transplant data from UNOS SRTR----

tx<-list()

tx[["kidney"]] <- load_srtr_file("tx_ki")
tx[["liver"]] <- load_srtr_file("tx_li")
tx[["heart"]] <- load_srtr_file("tx_hr")
tx[["lung"]] <- load_srtr_file("tx_lu")
tx[["pancreas"]] <- load_srtr_file("tx_kp")



donor_deceased <- load_srtr_file("donor_deceased")%>%
  select(DONOR_ID, DON_ANTI_HIV, DON_HIV_NAT)


#----Load transplant center data from SRTR package----
Transplant_centers_all<-list()

for (organ_loop in organ_list)
{
  

  Transplant_centers_all[[organ_loop]]<-sRtr::get_hrsa_transplant_centers()%>%
    filter(OPO_STATE_ABBR != "PR" & OPO_STATE_ABBR!= "HI")%>%
    select(OTC_NM, OTC_CD, Service_Lst, X, Y) %>%
    filter(str_detect(Service_Lst, str_to_sentence(organ_loop)))%>%
    select(-Service_Lst)%>%
    distinct()%>%
    rename(OTCName=OTC_NM, OTCCode=OTC_CD, Latitude=Y, Longitude=X)
  
  
  
}





#----Load census data----
us_tracts_population<-list()
us_counties_population<-list()
vars<-list()

for (year_loop in year_list){
  
  
  tracts_path<-paste0("cache/us_tracts_population_",year_loop,".rds")
  counties_path<-paste0("cache/us_counties_population_",year_loop,".rds")
  
  #Pull US Census tract populations if cache file doesn't already exist
  
  if (file.exists(tracts_path)) {
    us_tracts_population[[year_loop]] <- readRDS(tracts_path)
  } else {
    
    us_tracts_population[[year_loop]]<-get_acs(
      geography = "tract",
      variables = "B01003_001",
      year = year_loop,
      survey = "acs5",
      geometry = TRUE,
      state = continental_fips
    )%>%
      mutate(county_fips = substr(GEOID, 1, 5))%>%
      mutate(GEOID=as.numeric(GEOID))%>%
      mutate(county_fips=as.numeric(county_fips))%>%
      select(-variable, -moe)%>%
      rename(tract_population=estimate)
    
    saveRDS(us_tracts_population[[year_loop]], 
            tracts_path)
    
  }
  
  #Pull US Census counties populations if cache file doesn't already exist
  
  if (file.exists(counties_path)) {
    us_counties_population[[year_loop]] <- readRDS(counties_path)
  } else {
    
    us_counties_population[[year_loop]]<-get_acs(
      geography = "county",
      variables = "B01003_001",
      year = year_loop,
      survey = "acs5",
      geometry = TRUE,
      state = continental_fips
    )%>%
      mutate(GEOID=as.numeric(GEOID))%>%
      select(-variable, -moe)%>%
      rename(county_population=estimate)
    
    saveRDS(us_counties_population[[year_loop]], 
            counties_path)
    
    
  }
  
  # Load tract variable labels for age/sex values B01001 from the ACS 5-year 2017 dataset
  vars[[year_loop]] <- load_variables(year_loop, "acs5", cache = TRUE) %>%
    filter(str_detect(name, "B01001_"))%>%
    mutate(label = str_replace_all(label, "Estimate!!Total!!", ""))%>%
    mutate(label = str_replace_all(label, "!!", "_"))%>%
    mutate(label = str_replace_all(label, " ", "_"))
  
}



#Atlasplus 2022 and 2017 county totals

AtlasPlusTableData_2022_county_totals <- read_csv("CDC data folder/AtlasPlusTableData 2022 and 2017 county totals.csv", 
                                                  col_types = cols(Indicator = col_skip(), 
                                                                   Year = col_integer(), FIPS = col_number(), 
                                                                   Cases = col_number(), `Rate per 100000` = col_number()))%>%
  clean_names()%>%
  rename(county_cases=cases)%>%
  rename(geo_id=fips)%>%
  rename(county_name=geography)%>%
  mutate(county_cases=if_else(is.na(county_cases),0, county_cases))%>%
  filter(year==2022)%>%
  select(-year)

AtlasPlusTableData_2017_county_totals <- read_csv("CDC data folder/AtlasPlusTableData 2022 and 2017 county totals.csv", 
                                                  col_types = cols(Indicator = col_skip(), 
                                                                   Year = col_integer(), FIPS = col_number(), 
                                                                   Cases = col_number(), `Rate per 100000` = col_number()))%>%
  clean_names()%>%
  rename(county_cases=cases)%>%
  rename(geo_id=fips)%>%
  rename(county_name=geography)%>%
  mutate(county_cases=if_else(is.na(county_cases),0, county_cases))%>%
  filter(year==2017)%>%
  select(-year)


